--{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE OverloadedLists   #-}
--{-# LANGUAGE TypeFamilies #-}

import Data.Maybe
import System.Environment

import Tools
import Automata
import JSONType
import PreprocLexer
import PreprocParser
import PreprocSetup
import PreprocAST
import Preproc

main :: IO ()
main = do
    [dataFile, codeFile] <- getArgs
    rawData <- readFile dataFile
    rawCode  <- readFile codeFile
    let parsed = parse rawData rawCode
    putStrLn $ fromMaybe "Parse Error" parsed
