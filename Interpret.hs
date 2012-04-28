module Main where

import Lexfunk
import Parfunk
import Absfunk
import Interpreter

import ErrM

main = do
	interact calc
	putStrLn ""

calc s =
	case pProg (myLexer s) of
		Ok p -> show $ run p 
		e -> show e
