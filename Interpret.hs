module Main where

import LexFunk
import ParFunk
import AbsFunk
import Interpreter
import TypeChecker

import ErrM

main = do
	interact calc
	putStrLn ""

calc s =
	case pProg (myLexer s) of
		Ok p -> let typecheck = checkTypes p in case typecheck of
			Ok _ -> let out = run p in case out of
				Ok (t,v) -> show v
				Bad err -> show $ "RUN ERROR: " ++ err
			Bad e -> show $ "TYPE ERROR: " ++ e
		Bad e -> show $ "PARSE ERROR: " ++  e
