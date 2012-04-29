module TypeChecker where

import AbsFunk
import ErrM
import Model
import FunkStdLib
import qualified Data.Map as Map  


checkDec :: (Env, Store) -> Sign -> [Sign] -> Exp -> Err (Env, Store)
checkDec es (Sign t id) args exp = return es 

checkDecs :: (Env, Store) -> [Decl] -> Err (Env, Store)
checkDecs es decs = case decs of
	[] -> return es
	[(Declaration sign args exp)] -> checkDec es sign args exp
	h:t -> do
		nes <- checkDecs es [h]
		checkDecs nes t

checkTypes :: Prog -> Err Bool  
checkTypes (Program decs t exp) = let std = insertStd (Map.empty, Map.empty) in do
	es <- checkDecs std decs
	check es exp

check :: (Env, Store) -> Exp -> Err Bool
check es@(env, st) exp = return True

