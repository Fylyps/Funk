module Model where

import AbsFunk
import Data.Map
import Data.List
import ErrM

data Value = VInt Integer 
	   | VList [Value] 
	   | VTuple (Maybe Value) (Maybe Value) -- if left is nothing then its () 
	   | VFun {f :: Value -> Err Value}

tuple2List t = case t of
	VTuple Nothing Nothing -> []
	VTuple (Just x) Nothing -> [x]
	VTuple (Just x) (Just y) -> x:(tuple2List y)

instance Show Value where
	show (VInt i) = show i
	show (VFun _) = "<fun>"
	show (VList t) = "[" ++ intercalate "," (Prelude.map show t) ++ "]"
	show v@(VTuple a b) = "(" ++ intercalate "," list ++ ")"
		where list = Prelude.map show $ tuple2List v

type Env = Map Ident Value

lookFor :: Env -> Ident -> Err Value
lookFor env id = case Data.Map.lookup id env of
	Just v -> return v
	Nothing -> fail "not defined" 
