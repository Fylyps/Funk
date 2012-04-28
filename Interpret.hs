module Main where

import LexFunk
import ParFunk
import AbsFunk
import Interpreter

import ErrM

main = do
	interact calc
	putStrLn ""

calc s =
	case pProg (myLexer s) of
		Ok p -> let out = run p in case out of
			Ok out -> show out
			Bad err -> show $ "RUN ERROR: " ++ err
		Bad e -> show $ "PARSE ERROR: " ++  e
