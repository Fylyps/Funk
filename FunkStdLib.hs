
module FunkStdLib where

import AbsFunk
import Model
import Data.Map
import ErrM

insertStd :: Env -> Env
insertStd env =
	insertMany env [((Ident "plus"), (op2 plus)), ((Ident "minus"),(op2 minus)), ((Ident "times"),(op2 times)), ((Ident "divide"),(op2 divide)), 
			((Ident "eq"),(op2 eq)), ((Ident "neq"),(op2 neq)), ((Ident "inv"),(op1 inv)), 
			((Ident "gt"),(op2 gt)), ((Ident "lt"),(op2 lt))]

op1 f = VFun (\a -> f a)
op2 f = VFun (\a -> return $ VFun (\b -> f a b)) 
-- two arguments operator should be of type :: Value -> Value -> Err Value

plus a b = case (a,b) of
	(VInt x, VInt y) -> return $ VInt (x + y)
	_ -> fail "unable to add"

minus a b = case (a,b) of
	(VInt x, VInt y) -> return $ VInt (x - y)
	_ -> fail "unable to subtract"

times a b = case (a,b) of
	(VInt x, VInt y) -> return $ VInt (x*y)
	_ -> fail "unable to multiply"

divide a b = case (a,b) of
	(VInt x, VInt y) -> return $ VInt (div x y)
	_ -> fail "unable to divide"

eq a b = return $ VBool (a == b)
neq a b = return $ VBool (a /= b)
gt a b = case (a,b) of
	(VInt x, VInt y) -> return $ VBool (x > y)
	_ -> fail "unable to compare"
lt a b = gt b a

inv a = case a of
	(VInt x) -> return $ VInt (-x)
	(VBool b) -> return $ VBool (not b)
	_ -> fail "unable to inverse"

