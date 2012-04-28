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
		Ok p -> show $ run p 
		e -> show e
