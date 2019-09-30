{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}

module PreprocParser where

import Tools
import Automata
import JSONType
import PreprocSetup

type PreprocParser = Parser StackDomain ParserStateDomain TokenDomain OutputDomain

exprParser :: PreprocParser
exprParser = setTransition (define exprParserDef) initial

exprParserDef :: TransitionDefinition PreprocParser
exprParserDef (topOp, state, input', output) = case input' of
    Symbol input
        -- Push value to the output:
        |  state == State ExState
        && isAtom input
        -> ([topOp], State OpState, (atom input :) <$> output)
        -- Push prioritized binary infix or binary circumfix operator to the stack:
        |  state   == State OpState
        && topOp   << infixOps ++ prefixOps
        && inputOp << infixOps ++ binCircumfixOps
        && inputOp <| topOp
        -> ([inputOp, topOp], State ExState, output)
        -- Push right associative infix operator to the stack:
        |  state   == State OpState
        && topOp   << infixOps ++ prefixOps
        && inputOp << infixOps
        && inputOp <> topOp
        && assoc topOp   == RightAssoc
        && assoc inputOp == RightAssoc
        -> ([inputOp, topOp], State ExState, output)
        -- Push unary prefix operator to the stack:
        |  state   == State ExState
        && inputOp << prefixOps
        -> ([inputOp, topOp], State ExState, output)
        -- Push unary circumfix operator to the stack:
        |  state   == State ExState
        && inputOp << circumfixOps
        -> ([inputOp, topOp], State OpenState, output)
        -- Push binary infix or binary circumfix operator to the stack:
        |  state   == State OpState
        && inputOp << infixOps ++ binCircumfixOps
        && topOp   </ infixOps ++ prefixOps
        -> ([inputOp, topOp], State ExState, output)
        -- Perform binary circumfix operation:
        |  state   == State OpState
        && topOp   << binCircumfixOps
        && input   == closingToken topOp
        -> ([], State OpState, onHead2 (binOperation topOp) <$> output)
        -- Close empty circumfix operator:
        |  state   == State OpenState
        && input   << closingTokens
        && topOp   == openingOp input
        && inserts topOp
        -> ([topOp], closingState $ openingOp input, closingInsert (openingOp input) <$> output)
        -- Close circumfix operator:
        |  state   == State OpState
        && input   << closingTokens
        && topOp   << [openingOp input, pure Comma]
        -> (closingInsertComma (openingOp input) [topOp], closingState $ openingOp input, closingInsert (openingOp input) <$> output)
        where
            inputOp = op input state
    EmptySymbol
        -- Initialization:
        |  state  == InitState
        && output == NoOutput
        -> ([topOp], State ExState, Output [])
        |  state == InitState
        -> ([topOp], State ExState, output)
        -- Return:
        |  topOp == InitSymbol
        -> ([topOp], backTo (State ExprState), output)
        -- Perform circumfix operation:
        |  state << closingStates
        && topOp == openStateOp state
        -> ([], State OpState, onHead (operation topOp) <$> output)
        -- Perform binary infix operation:
        |  topOp << infixOps
        -> ([], state, onHead2 (binOperation topOp) <$> output)
        -- Perform unary prefix operation:
        |  topOp << prefixOps
        -> ([], state, onHead (operation topOp) <$> output)
        -- Unary circumfix operator is not empty:
        |  state == State OpenState
        && topOp << circumfixOps
        -> ([topOp], State ExState, output)
        -- Error:
    _   -> ([topOp], FailState, output)
    where
        infix 4 <|; (<|) = prior
        infix 4 |>; (|>) = flip prior
        infix 4 <>; (<>) = eqPrior
        closingStatesMap'   = closingStatesMap :: [(StackSymbol StackDomain, State PreprocParser)]
        opMap'              = opMap            :: [(State PreprocParser, TokenDomain, StackSymbol StackDomain)]
        closingTokens       = [x | (_, x) <- closingTokensMap]
        closingStates       = [x | (_, x) <- closingStatesMap]
        ops                 = prefixOps ++ infixOps ++ circumfixOps ++ binCircumfixOps
        op    token   state = headOr FailSymbol      [x | (state', token', x) <- opMap',            state' == state, token' == token]
        openStateOp   state = headOr FailSymbol      [x | (x, state')         <- closingStatesMap,  state' == state]
        openingOp     token = headOr FailSymbol      [x | (x, token')         <- closingTokensMap,  token' == token]
        closingToken     op = headOr UnknownToken    [x | (op', x)            <- closingTokensMap,     op' == op]
        closingState     op = headOr FailState       [x | (op', x)            <- closingStatesMap,     op' == op]
        closingInsertion op = headOr Error           [x | (op', x)            <- closingInsertionsMap, op' == op]
        operation        op = headOr (const Error)   [x | (op', x)            <- operationsMap,        op' == op]
        binOperation     op = headOr (\x y -> Error) [x | (op', x)            <- binOperationsMap,     op' == op]
        assoc         token = headOr LeftAssoc       [x | (token', x)         <- associativities,   token' == token]
        inserts op = case closingInsertion op of
            Error -> False
            _     -> True
        closingInsert op o
            | inserts op = closingInsertion op : o
            | otherwise  = o
        closingInsertComma op s
            | inserts op = pure Comma : s
            | otherwise  = s
        eqPrior x y = priority x == priority y
        prior   x y = priority x < priority y
        priority    = priority' 0 priorities
            where
                priority' i (first : rest) op
                    | op << first = i
                    | otherwise   = priority' (i + 1) rest op
                priority' i [] _ = i + 1
        isAtom NameToken    {} = True
        isAtom LiteralToken {} = True
        isAtom _               = False
        atom  (NameToken    x) = Single $ Name x
        atom  (LiteralToken x) = Single $ Literal x
        atom   _               = Error

{-

lorem
[: for <name> in <expr> :]
    ipsum
    [: let <name> = <expr>; <name> = <expr> :]
        dolor
        [: if <expr> :]
            sit
        [: elif <expr> :]
            amet
        [: else <expr> :]
            adipiscing
            [=<expr>]
            bar
        [: end :]
        elit
    [: end :]
    est
[: end :]
foo

-}


{-

stack   state   input   ->  stack       state   output
-------|-------|-------|---|-----------|-------|------
        #body   $cont       &cont       #body   (content input :) <$> output
        #body   $[:                     #st

        #st     $for                    #for
        #st     $if                     #if
        #st     $elif                   #elif
        #st     $else                   #else
        #st     $let                    #let
        #st     $=                      #inj
        #st     $end                    #end

        #for    $name       &for        #in     (name input :) <$> output
        #in     $in                     #dom
        #dom    \                       =>expr
&for    #expr   $:]                     #body   onHead2 forHead <$> output

        #if     \           &if         =>expr
&if     #expr   $:]                     #body   onHead ifHead <$> output
        #elif   \           &elif       =>expr
&elif   #expr   $:]                     #body   onHead elifHead <$> output
        #else   \           &else       =>expr
&else   #expr   $:]                     #body   onHead elseHead <$> output

        #let    $name       &let        #var    (name input :) <$> output
        #var    $=          &ass        =>expr
&ass    #expr   $:]                     #cLet   (Declarations [] :) . onHead2 declare <$> output
&ass    #expr   $;                      #let'   onHead2 declare <$> output
        #let'   $name                   #var    (name input :) <$> output
&ass    #cLet   \           pop         #cLet   onHead2 pushDecl <$> output
&let    #cLet   \                       #body   onHead letHead <$> output

        #inj    \           &inj        =>expr
&inj    #expr   $:]         pop; &st    #body   onHead injectExpr <$> output

        #end    $:]                     #close  (Body [] :) <$> output

&cont   #close  \                       #close  onHead2 pushBody <$> output
&cont   #cIf    \                       #cIf    onHead2 pushBody <$> output
&st     #close  \                       #close  onHead2 pushBody <$> output
&st     #cIf    \                       #cIf    onHead2 pushBody <$> output

&for    #close  \           pop         #body   onHead2 forExpr <$> output

&if     #close  \           pop         #body   onHead2 ifExpr <$> output
&if     #cIf    \           pop         #body   onHead2 ifExpr <$> output
&elif   #close  \                       #cIf    (Body [] :) . onHead elifExpr <$> output
&elif   #cIf    \                       #cIf    (Body [] :) . onHead elifExpr <$> output
&else   #close  \                       #cIf    (Body [] :) . onHead elseExpr <$> output

&let    #close  \           pop         #body   onHead2 let <$> output
 


-}

pushBody        :: OutputDomain' -> OutputDomain' -> OutputDomain'
stmtState       :: TokenDomain -> State PreprocParser
keyVal          :: String -> OutputDomain' -> OutputDomain'
forHead         :: OutputDomain' -> OutputDomain' -> OutputDomain'
forStmt         :: OutputDomain' -> OutputDomain' -> OutputDomain'
declare         :: OutputDomain' -> OutputDomain' -> OutputDomain'
pushDecl        :: OutputDomain' -> OutputDomain' -> OutputDomain'
declStmt        :: OutputDomain' -> OutputDomain' -> OutputDomain'
injStmt         :: OutputDomain' -> OutputDomain'
cond            :: OutputDomain' -> OutputDomain' -> OutputDomain'
pushCond        :: OutputDomain' -> OutputDomain' -> OutputDomain'
condStmt        :: OutputDomain' -> OutputDomain'
rootStmt        :: OutputDomain' -> OutputDomain'

statementStates' = statementStates :: [(TokenDomain, State PreprocParser)]

pushBody (Content'   cont) (Body body) = Body $ ContentBody   cont : body
pushBody (Statement' stmt) (Body body) = Body $ StatementBody stmt : body
pushBody _ _ = Error
stmtState token = headOr FailState [x | (token', x) <- statementStates', token' == token]
keyVal val (Name' key) = NamePair' key val
keyVal _ _ = Error
forHead (Name' var)         (Single expr) = ForHead Nothing    var expr
forHead (NamePair' key val) (Single expr) = ForHead (Just key) val expr
forHead _ _ = Error
forStmt (ForHead key val dom) (Body body) = Statement' $ LoopStatement key val dom body
forStmt _ _ = Error
declare (Name' var) (Single expr) = Declaration var expr
declare _ _ = Error
pushDecl (Declaration var expr) (Declarations decls) = Declarations $ (var, expr) : decls
pushDecl _ _ = Error
declStmt (Declarations decls) (Body body) = Statement' $ DeclarationStatement decls body
declStmt _ _ = Error
injStmt (Single expr) = Statement' $ InjectionStatement expr
injStmt _ = Error
cond (Single expr) (Body body) = Condition' $ Condition expr body
cond _ _ = Error
pushCond (Condition' cond) (Conditions conds) = Conditions $ cond : conds
pushCond _ _ = Error
condStmt (Conditions conds) = Statement' $ ConditionStatement conds
condStmt _ = Error
rootStmt (Body body) = Statement' $ RootStatement Null body
rootStmt _ = Error

preprocParser :: PreprocParser
preprocParser = setTransition (define parserDef) initial

parserDef :: TransitionDefinition PreprocParser
parserDef (top, state, input', output) = case input' of
    -- Content:
    Symbol (ContentToken content)
        |  state == State BodyState
        -> ([pure Content, top], State BodyState, (Content' content :) <$> output)
    
    -- Statements:
    Symbol input
        |  state == State BodyState
        && input << statementTokens
        -> ([top], stmtState input, output)

    -- For:
    Symbol (NameToken name)
        |  state == State ForState
        -> ([pure For, top], State InState, (Name' name :) <$> output)
        |  state == State KeyValState
        -> ([top], State InState, onHead (keyVal name) <$> output)
    Symbol CommaToken
        |  state == State InState
        -> ([top], State KeyValState, output)
    Symbol InToken
        |  state == State InState
        -> ([top], State DomState, output)
    EmptySymbol
        |  state == State DomState
        -> ([top], continueTo exprParser, output)
        |  state == State ExprState
        && top   == pure For
        -> ([top], State BodyState, onHead2 forHead <$> output)

    -- If..elif..else:
    EmptySymbol
        |  state == State IfState
        -> ([pure If, top], continueTo exprParser, output)
        |  state == State ExprState
        && top   == pure If
        -> ([top], State BodyState, output)
        |  state == State ElifState
        -> ([pure Elif, top], continueTo exprParser, output)
        |  state == State ExprState
        && top   == pure Elif
        -> ([top], State BodyState, output)
        |  state == State ElseState
        -> ([pure Else, top], State BodyState, (Single (Computed true) :) <$> output)

    -- Let:
    Symbol (NameToken name)
        |  state == State LetState
        -> ([pure Let, top], State VarState, (Name' name :) <$> output)
        |  state == State LetState'
        -> ([top], State VarState, (Name' name :) <$> output)
    Symbol AssToken
        |  state == State VarState
        -> ([pure Ass, top], continueTo exprParser, output)
    Symbol SemicolonToken
        |  state == State ExprState
        && top   == pure Ass
        -> ([top], State LetState', onHead2 declare <$> output)
    EmptySymbol
        |  state == State ExprState
        && top   == pure Ass
        -> ([], State CloseLetState, (Declarations [] :) . onHead2 declare <$> output)
        |  state == State CloseLetState
        && top   == pure Ass
        -> ([], State CloseLetState, onHead2 pushDecl <$> output)
        |  state == State CloseLetState
        && top   == pure Let
        -> ([top], State BodyState, onHead2 pushDecl <$> output)
    
    -- Injection:
    EmptySymbol
        |  state == State InjectionState
        -> ([pure Inj, top], continueTo exprParser, output)
        |  state == State ExprState
        && top   == pure Inj
        -> ([pure Statement], State BodyState, onHead injStmt <$> output)

    -- End:
    EmptySymbol
        |  state == State EndState'
        -> ([top], State CloseState, (Body [] :) <$> output)
        |  state == State ElifState'
        -> ([top], State ElifCloseState, (Body [] :) <$> output)
        |  state == State ElseState'
        -> ([top], State ElseCloseState, (Body [] :) <$> output)

    -- Closing blocks:
    EmptySymbol
        |  state << map State [CloseState, ElifCloseState, ElseCloseState]
        && top   << map pure  [Content, Statement]
        -> ([], state, onHead2 pushBody <$> output)
        
        |  state == State CloseState
        && top   == pure For
        -> ([pure Statement], State BodyState, onHead2 forStmt <$> output)
        
        |  state == State CloseState
        && top   == pure Let
        -> ([pure Statement], State BodyState, onHead2 declStmt <$> output)

        |  state == State CloseState
        && top   == pure If
        -> ([pure Statement], State BodyState, onHead condStmt . onHead2 pushCond . (Conditions [] :) . onHead2 cond <$> output)
        |  state == State CloseState
        && top   == pure Elif
        -> ([], State CloseCondsState, (Conditions [] :) . onHead2 cond <$> output)
        |  state == State ElifCloseState
        && top   == pure If
        -> ([pure IfCond], State ElifState, onHead2 cond <$> output)
        |  state == State ElseCloseState
        && top   == pure If
        -> ([pure IfCond], State ElseState, onHead2 cond <$> output)
        |  state == State ElifCloseState
        && top   == pure Elif
        -> ([pure Cond], State ElifState, onHead2 cond <$> output)
        |  state == State ElseCloseState
        && top   == pure Elif
        -> ([pure Cond], State ElseState, onHead2 cond <$> output)
        |  state == State CloseState
        && top   == pure Else
        -> ([pure Cond], State CloseCondsState, (Conditions [] :) . onHead2 cond <$> output)
        |  state == State CloseCondsState
        && top   == pure Cond
        -> ([], State CloseCondsState, onHead2 pushCond <$> output)
        |  state == State CloseCondsState
        && top   == pure IfCond
        -> ([pure Statement], State BodyState, onHead condStmt . onHead2 pushCond <$> output)
    
    EndSymbol
        |  state == State BodyState
        -> ([top], State CloseState, (Body [] :) <$> output)
    EmptySymbol
        |  state == State CloseState
        && top   == InitSymbol
        -> ([pure Statement, top], EndState, onHead rootStmt <$> output)

    EmptySymbol
        |  state == InitState
        -> ([top], State BodyState, Output [])
    
    _ -> ([top], FailState, output)
