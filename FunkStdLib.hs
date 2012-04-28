
module FunkStdLib where

import AbsFunk
import Model
import Data.Map

insertStd :: Env -> Env
insertStd env =
	insert (Ident "plus") (plusValue) env


plus a b = case (a,b) of
	(Vint x, Vint y) -> Vint (x + y)
	
plusValue = Vfun (\a -> Vfun (\b -> plus a b)) 



