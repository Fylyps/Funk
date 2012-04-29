module Model where

import AbsFunk
import Data.Map
import Data.List
import ErrM

data Value = VInt Integer 
	   | VList [Value] 
	   | VTuple (Maybe Value) (Maybe Value) -- if left is nothing then its () 
	   | VFun {f :: Value -> Err Value}
	   | VBool Bool

instance Eq Value where
	(VBool x) == (VBool y) = x == y
	(VInt x)    == (VInt y)    = x == y
	(VList x) == (VList y) = x == y
	VTuple a b == VTuple c d = a == c && b == d
	_ == _ = False

tuple2List t = case t of
	VTuple Nothing Nothing -> []
	VTuple (Just x) Nothing -> [x]
	VTuple (Just x) (Just y) -> x:(tuple2List y)

instance Show Value where
	show (VBool b) = show b
	show (VInt i) = show i
	show (VFun _) = "<fun>"
	show (VList t) = "[" ++ intercalate "," (Prelude.map show t) ++ "]"
	show v@(VTuple a b) = "{" ++ intercalate "," list ++ "}"
		where list = Prelude.map show $ tuple2List v

type Loc = Int
type Env = Map Ident Loc
type Store = Map Loc Value

nextLoc :: Store -> Loc
nextLoc st = (size st) + 1

lookFor :: (Env, Store) -> Ident -> Err Value
lookFor (env, st) id = case Data.Map.lookup id env of
	Just loc -> case Data.Map.lookup loc st of
		Just v -> return $ v
		Nothing -> case id of
			Ident name -> fail $ "wrong location: " ++ (show loc) ++ " while performing search of " ++ name
	Nothing -> case id of
		Ident name -> fail $ (show name) ++ " not defined"

 
ienv :: (Env, Store) -> Ident -> Value -> (Env, Store)
ienv (env, st) id val = let loc = nextLoc st in	
			 let nst = Data.Map.insert loc val st in
			  let nenv = Data.Map.insert id loc env in
			   (nenv, nst)

insertMany :: (Env, Store) -> [(Ident, Value)] -> (Env, Store)
insertMany = foldl (\es (i,v) -> ienv es i v)


