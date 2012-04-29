
module FunkStdLib where

import AbsFunk
import Model
import Data.Map
import ErrM

insertStd :: (Env, Store) -> (Env, Store)
insertStd es =
	insertMany es [ ((Ident "plus"),(op2 TInt (VInt) (+))),
			((Ident "minus"),(op2 TInt (VInt) (-))), 
			((Ident "times"),(op2 TInt (VInt) (*))), 
			((Ident "divide"),(op2 TInt (VInt) (div))),
			((Ident "inv"),(op1 TInt (VInt) (0-))),
			((Ident "eq"),(op2 TBool (VBool) (==))),
			((Ident "neq"),(op2 TBool (VBool) (/=))),
			((Ident "gt"),(op2 TBool (VBool) (>))), 
			((Ident "lt"),(op2 TBool (VBool) (<)))
			]


op1 outType constr op = (TFun TInt outType, VFun (\s a-> ((createOp1 outType constr op) a, s)))
op2 outType constr op = (TFun TInt (TFun TInt outType), VFun (\s a -> 
		(return $ ((TFun TInt outType),VFun (\s b -> ((createOp2 outType constr op) a b, s))),s)))

createOp1 t constr op a = case a of
	(TInt, VInt x) -> return $ (t, constr (op x))
	_ -> fail "unable to use operator"

createOp2 t constr op a b = case (a,b) of
	((TInt, VInt x), (TInt,VInt y)) -> return $ (t, constr (op x y))
	_ -> fail "unable to use operator"

