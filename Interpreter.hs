module Interpreter where

import AbsFunk
import ErrM
import Model
import FunkStdLib
import qualified Data.Map as Map  

buildValue :: Env -> [Ident] -> Exp -> Value
buildValue env args exp = case args of						
	[] -> eval env exp
	h:t -> Vfun (\v -> buildValue (Map.insert h v env) t exp)

ienv :: Env -> Decl -> Env
ienv env (Declaration id args exp) = Map.insert id val env 
					where val = buildValue env args exp


run :: Prog -> Value
run (Program decs exp) = eval env exp 
				where env = foldl ienv std decs 
					where std = insertStd Map.empty

apply :: Env -> Value -> [Exp] -> Value
apply env val args = case args of
	[] -> val
	h:t -> case val of
		(Vfun f) -> let argval = eval env h in 
			    let applied = f argval in
				apply env applied t -- prawdopodobnie trzeba poprawic srodowisko

eval :: Env -> Exp -> Value
eval env e = case e of
	EGet id -> apply env (env Map.! id) []
	EApplication id args -> apply env (env Map.! id) args
	ELet d exp -> 	eval (ienv env d) exp
	ECase exp patts -> Vint 101
	EList exps -> VList (map (eval env) exps)
	EInt n -> Vint n
