{-# LANGUAGE TypeFamilies #-}

module Preproc where

import Data.Maybe

import Tools
import JSONType
import Automata
import PreprocSetup
import PreprocLexer
import PreprocParser
import PreprocAST

dropContent (ContentToken {} : rest) = trimContent rest
dropContent x = x
trimContent = reverse . dropContent . reverse . dropContent

lex' s = run $ load (map pure s) lexer

expressionParser s = trimmed >>> exprParser
    where
        lexed   = lex' s
        trimmed = setOutput (trimContent <$> output lexed) lexed 

parseExpression s = case (state parsed, output parsed, input parsed) of
    (Return {}, Output [Single e], []) -> Just $ eval [] e
    (EndState,  Output [Single e], []) -> Just $ eval [] e
    _ -> Nothing
    where
        parsed = run $ expressionParser $ "|" ++ map repl s
        repl '\n' = ' '
        repl x = x
    
parser s = lexed >>> preprocParser
    where
        lexed = run $ load (map pure s) lexer

parse' d s = case (state parsed, output parsed) of
    (EndState, Output [Statement' (RootStatement _ body)]) -> Just $ exec [] (RootStatement d body)
    _ -> Nothing
    where
        parsed = run $ parser s

parse z = parse' (fromMaybe Null $ parseExpression z)