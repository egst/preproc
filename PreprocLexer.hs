{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}

module PreprocLexer where

import Data.Char (ord)

import Tools
import Automata
import JSONType
import PreprocSetup

type PreprocLexer = Lexer LexerStateDomain TokenDomain

lexer :: PreprocLexer
lexer = setTransition (define lexerDef) initial

lexerDef :: TransitionDefinition PreprocLexer
lexerDef (state, input', output) = case input' of
    Symbol input
        -- Skip whitespace:
        |  state == State CodeState
        && input << whitespaceSymbols
        -> (State CodeState, output)
        -- Possible end of code:
        |  state == State CodeState
        && input == '\\'
        -> (State EscapeState', output)
        |  state == State CodeState
        && input == ';'
        -> (State EscapeState'', output)
        |  state == State EscapeState'
        && input == '\n'
        -> (State CodeState, output)
        |  state == State EscapeState''
        && input /= '|'
        -> (State CodeState, (SemicolonToken :) <$> output)
        |  state == State CodeState
        && input == '|'
        -> (State PipeState', output)
        |  state == State PipeState'
        && input == '|'
        -> (State CodeState, (OrToken :) <$> output)
        |  state == State PipeState'
        && input /= '|'
        -> (State ContentState, (ContentToken [input] :) <$> output)
        |  state == State CodeState
        && input == '\n'
        -> (State ContentState, (ContentToken [] :) <$> output)
    EndSymbol
        |  state << map State [PipeState', CodeState]
        -> (EndState, reverse <$> output)
    Symbol input
        -- Possible beginning of code:
        |  state == State ContentState
        && input == '|'
        -> (State PipeState, output)
        |  state == State PipeState
        && input == '|'
        -> (State CodeState, onHead (pushContent '|' . pushContent '|') <$> output)
    EmptySymbol
        -- Code begins, content ends:
        |  state == State PipeState
        -> (State CodeState, onHead reverseContent <$> output)
    Symbol input
        -- Reading content:
        {-|  state == State OBrackState
        -> (State ContentState, onHead (pushContent input . pushContent '[') <$> output)-}
        |  state == State ContentState
        -> (State ContentState, onHead (pushContent input) <$> output)
        -- Reading single-symbol operator:
        |  state == State CodeState
        && input << singleOpSymbols
        -> (State CodeState, (singleOpToken input :) <$> output)
        -- Reading first symbol of possibly double-symbol operator:
        |  state == State CodeState
        && input << doubleOpSymbols
        -> (opState input, output)
        -- Reading second symbol of double-symbol operator:
        |  state << doubleOpStates
        && input << secondOpSymbols
        -> (State CodeState, (doubleOpToken state input :) <$> output)
        -- Reading double-symbol operator after first symbol of another double-symbol operator:
        |  state << doubleOpStates
        && input << doubleOpSymbols
        -> (opState input, ((singleOpToken $ opSymbol state) :) <$> output)
        -- Reading single-symbol operator after first symbol of a double-symbol operator:
        |  state << doubleOpStates
        && input << singleOpSymbols
        -> (State CodeState, (singleOpToken input :) . ((singleOpToken $ opSymbol state) :) <$> output)
        -- Reading whitespace after first symbol of a double-symbol operator:
        |  state << doubleOpStates
        && input << whitespaceSymbols
        -> (State CodeState, ((singleOpToken $ opSymbol state) :) <$> output)
        -- Reading initial name symbol after first symbol of a double-symbol operator:
        |  state << doubleOpStates
        && input << initNameSymbols
        -> (passTo keywordsAndNamesLexer, ((singleOpToken $ opSymbol state) :) <$> output)
        -- Reading initial literal symbol after first symbol of a double-symbol operator:
        |  state << doubleOpStates
        && input << digitSymbols ++ "\""
        -> (passTo literalsLexer, ((singleOpToken $ opSymbol state) :) <$> output)
        -- Reading initial name symbol:
        |  state == State CodeState
        && input << initNameSymbols
        -> (passTo keywordsAndNamesLexer, output)
        -- Reading initial literal symbol:
        |  state == State CodeState
        && input << digitSymbols ++ "\""
        -> (passTo literalsLexer, output)
    EndSymbol
        -- Finishing with content:
        |  state == State OBrackState
        -> (State ContentState, onHead (pushContent '[') <$> output)
        |  state == State ContentState
        -> (EndState, reverse . onHead reverseContent <$> output)
    EmptySymbol
        -- Initialization:
        |  state == InitState
        -> (State ContentState, Output [ContentToken ""])
        
    _   |  state == State EndState''
        -> (EndState, reverse <$> output)
        -- Error:
        |  otherwise
        -> (FailState, output)

    where
        pushContent    c (ContentToken s) = ContentToken $ c : s
        pushContent    _ x                = x
        reverseContent   (ContentToken s) = ContentToken $ reverse s
        reverseContent   x                = x

        firstOps'  = firstOps  :: [(Char, State PreprocLexer, TokenDomain)]
        secondOps' = secondOps :: [(State PreprocLexer, Char, TokenDomain)]
        singleOpSymbols = [symbol | (symbol, state, _) <- firstOps', state == State CodeState]
        doubleOpSymbols = [symbol | (symbol, state, _) <- firstOps', state /= State CodeState]
        secondOpSymbols = [symbol | (_, symbol, _)     <- secondOps']
        doubleOpStates  = [state  | (_, state, _)      <- firstOps', state /= State CodeState]

        opState symbol' = headOr FailState
            [state | (symbol, state, _) <- firstOps', symbol' == symbol]
        opSymbol state' = headOr '\0'
            [symbol | (symbol, state, _) <- firstOps', state' == state]
        singleOpToken symbol' = headOr UnknownToken
            [token | (symbol, _, token) <- firstOps', symbol' == symbol]
        doubleOpToken state' symbol' = headOr UnknownToken
            [token | (state, symbol, token) <- secondOps', state' == state, symbol' == symbol]
    
keywordsAndNamesLexer :: PreprocLexer
keywordsAndNamesLexer = setTransition (define keywordsAndNamesLexerDef) initial

keywordsAndNamesLexerDef :: TransitionDefinition PreprocLexer
keywordsAndNamesLexerDef (state, input', output) = case input' of
    Symbol input
        -- Reading keywords:
        |  (state, input) << keywordTransitionPairs
        -> (keywordState state input, output)
        -- Switch to reading a name:
        |  state << keywordStates
        && input << nameSymbols
        -> (State NameState, (pushNameChar input (keywordName state) :) <$> output)
        -- Reading initial name symbol:
        |  state == InitState
        && input << initNameSymbols
        -> (State NameState, (pushNameChar input (NameToken "") :) <$> output)
        -- Reading a name:
        |  state == State NameState
        && input << nameSymbols
        -> (State NameState, onHead (pushNameChar input) <$> output)
        -- Name ends:
        |  state == State NameState
        && input </ nameSymbols
        -> (backTo $ State CodeState, onHead reverseName <$> output)
        -- Keyword ends:
        |  state << finalKeywordStates
        && input </ nameSymbols
        -> (backTo $ State CodeState, (keywordToken state :) <$> output)
    EndSymbol
        -- Keyword ends:
        |  state << finalKeywordStates
        -> (backTo $ State EndState'', (keywordToken state :) <$> output)
        -- Switch to reading a name:
        |  state << keywordStates
        -> (backTo $ State EndState'', onHead reverseName . (keywordName state :) <$> output)
        -- Name ends:
        |  state == State NameState
        -> (backTo $ State EndState'', onHead reverseName <$> output)
        -- Error:
    _   -> (FailState, output)

    where
        pushNameChar c (NameToken s) = NameToken $ c : s
        pushNameChar _ x             = x
        reverseName    (NameToken s) = NameToken $ reverse s
        reverseName    x             = x

        keywordTransitionPairs = [(state, symbol) | (state, symbol, _) <- keywordTransitions]
        keywordStates          = [state           | (state, _)         <- keywordStrings]
        finalKeywordStates     = [state           | (state, _)         <- keywordTokens]

        keywordName state' = headOr UnknownToken
            [NameToken $ reverse string | (state, string) <- keywordStrings, state' == state]
        keywordToken state' = headOr UnknownToken
            [token | (state, token) <- keywordTokens, state' == state]
        keywordState state' symbol' = headOr FailState
            [result | (state, symbol, result) <- keywordTransitions, state' == state, symbol' == symbol]

literalsLexer :: PreprocLexer
literalsLexer = setTransition (define literalsLexerDef) initial

literalsLexerDef :: TransitionDefinition PreprocLexer
literalsLexerDef (state, input', output) = case input' of
    Symbol input
        -- Reading initial decimal point symbol:
        |  state == InitState
        && input == pointSymbol
        -> (State PointState, (LiteralToken (NumberLiteral 0 0 0) :) <$> output)
        -- Reading initial digit symbol:
        |  state == InitState
        && input << digitSymbols
        -> (State IntegralState, (LiteralToken (NumberLiteral (digit input) 0 0) :) <$> output)
        -- Reading decimal point symbol after dgits:
        |  state == State IntegralState
        && input == pointSymbol
        -> (State DecimalState, output)
        -- Reading digits before decimal point:
        |  state == State IntegralState
        && input << digitSymbols
        -> (State IntegralState, onHead (addIntegral $ digit input) <$> output)
        -- Reading digits after decimal point:
        |  state << [State DecimalState, State PointState]
        && input << digitSymbols
        -> (State DecimalState, onHead (addDecimal $ digit input) <$> output)
        -- Reading exponent symbol:
        |  state << [State IntegralState, State DecimalState]
        && input << exponentSymbols
        -> (State EState, output)
        -- Reading exponent digits:
        |  state << [State ExponentState, State EState]
        && input << digitSymbols
        -> (State ExponentState, onHead (addExponent $ digit input) <$> output)
        -- Finishing on valid number:
        |  state << [State IntegralState, State DecimalState, State ExponentState]
        -> (backTo $ State CodeState, output)
    EndSymbol
        |  state << [State IntegralState, State DecimalState, State ExponentState]
        -> (backTo EndState, output)
    Symbol input
        -- Reading initial quote symbol:
        |  state == InitState
        && input == quoteSymbol
        -> (State QuoteState, (LiteralToken (StringLiteral "") :) <$> output)
        -- Reading final quote symbol:
        |  state << [State StringState, State QuoteState]
        && input == quoteSymbol
        -> (returnTo $ State CodeState, onHead reverseString <$> output)
        -- Reading string value:
        |  state << [State StringState, State QuoteState]
        -> (State StringState, onHead (addChar input) <$> output)
        -- TODO: Escape
        -- Error:
    _   -> (FailState, output)

    where
        digit d = ord d - 48
        addIntegral   n (LiteralToken (NumberLiteral i d e)) = LiteralToken $ NumberLiteral (n + 10 * i) d e
        addIntegral   _ x                                    = x
        addDecimal    n (LiteralToken (NumberLiteral i d e)) = LiteralToken $ NumberLiteral i (n + 10 * d) e
        addDecimal    _ x                                    = x
        addExponent   n (LiteralToken (NumberLiteral i d e)) = LiteralToken $ NumberLiteral i d (n + 10 * e)
        addExponent   _ x                                    = x
        reverseString   (LiteralToken (StringLiteral s))     = LiteralToken $ StringLiteral $ reverse s
        reverseString   x                                    = x
        addChar       c (LiteralToken (StringLiteral s))     = LiteralToken $ StringLiteral $ c : s
        addChar       _ x                                    = x