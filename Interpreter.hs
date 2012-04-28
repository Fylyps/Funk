module Interpreter where

import Absfunk

import Data.Map

data Value = Vint Integer | Vfun {f :: Value -> Value}
instance Show Value where
	show (Vint i) = show i
	show (Vfun _) = "<fun>"

buildValue :: Env -> [Ident] -> Exp -> Value
buildValue env args exp = case args of						
	[] -> eval env exp
	h:t -> Vfun (\v -> buildValue (insert h v env) t exp)


type Env = Map Ident Value
ienv :: Env -> Decl -> Env
ienv env (Declaration id args exp) = insert id val env 
					where val = buildValue env args exp
run :: Prog -> Value
run (Program decs exp) = eval env exp where env = foldl ienv empty decs

apply :: Env -> Value -> [Exp] -> Value
apply env val args = case args of
	[] -> val
	h:t -> case val of
		(Vfun f) -> let argval = eval env h in 
			    let applied = f argval in
				apply env applied t -- prawdopodobnie trzeba poprawic srodowisko

eval :: Env -> Exp -> Value
eval env e = case e of
	EApplication id args -> apply env (env!id) args
	ELet d exp  -> 	eval (ienv env d) exp
	EInt n -> Vint n
