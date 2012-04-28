
module FunkStdLib where

import AbsFunk
import Model
import Data.Map
import ErrM

insertStd :: Env -> Env
insertStd env =
	insert (Ident "plus") (plusValue) env

plus :: Value -> Value -> Err Value
plus a b = case (a,b) of
	(VInt x, VInt y) -> return $ VInt (x + y)
	_ -> fail "unable to add"
	
plusValue = VFun (\a -> return $ VFun (\b -> plus a b)) 



