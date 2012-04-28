
module FunkStdLib where

import AbsFunk
import Model
import Data.Map
import ErrM

insertStd :: Env -> Env
insertStd env =
	insertMany env [((Ident "plus"), (op2 plus)), ((Ident "eq"),(op2 eq))]

op2 f = VFun (\a -> return $ VFun (\b -> f a b)) 

plus :: Value -> Value -> Err Value
plus a b = case (a,b) of
	(VInt x, VInt y) -> return $ VInt (x + y)
	_ -> fail "unable to add"
	
eq :: Value -> Value -> Err Value
eq a b = return $ VBool (a == b)

