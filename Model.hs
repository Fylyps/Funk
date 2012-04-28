module Model where

import AbsFunk
import Data.Map
import Data.List
import ErrM

data Value = VInt Integer | VList [Value] | VFun {f :: Value -> Err Value}
instance Show Value where
	show (VInt i) = show i
	show (VFun _) = "<fun>"
	show (VList t) = "[" ++ intercalate "," (Prelude.map show t) ++ "]"

type Env = Map Ident Value

lookFor :: Env -> Ident -> Err Value
lookFor env id = case Data.Map.lookup id env of
	Just v -> return v
	Nothing -> fail "not defined" 
