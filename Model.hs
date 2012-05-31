module Model where

import AbsFunk
import Data.Map
import Data.List
import ErrM

data Value = VInt Integer 
	   | VList [Value] 
	   | VTuple (Maybe Value) (Maybe Value) -- if left is nothing then its () 
	   | VFun ((Env, Store) -> TValue -> (Err TValue, (Env, Store))) (Env, Store) -- store from definition, need to add itself
	   | VBool Bool
	   | VNone

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
	show (VFun _ _) = "<fun>"
	show (VList t) = "[" ++ intercalate "," (Prelude.map show t) ++ "]"
	show v@(VTuple a b) = "{" ++ intercalate "," list ++ "}"
		where list = Prelude.map show $ tuple2List v

type Loc = Int
type Env = Map Ident Loc
type TValue = (Type, Value)
type Store = Map Loc TValue

nextLoc :: Store -> Loc
nextLoc st = (size st) + 1

lookFor :: (Env, Store) -> Ident -> Err TValue
lookFor (env, st) id = case Data.Map.lookup id env of
	Just loc -> case Data.Map.lookup loc st of
		Just v -> return $ v
		Nothing -> case id of
			Ident name -> fail $ "wrong location: " ++ (show loc) ++ " while performing search of " ++ name
	Nothing -> case id of
		Ident name -> fail $ (show name) ++ " not defined " ++ (showTree env) ++ "Sore: " ++ (showTree st)


emptyEnv = Data.Map.empty
emptyStore = Data.Map.empty

emptyES :: (Env, Store)
emptyES = (emptyEnv, emptyStore)

ienv :: Env -> Ident -> Loc -> Env
ienv env id loc = Data.Map.insert id loc env

ist :: Store -> Loc -> TValue -> Store
ist st loc val = Data.Map.insert loc val st
 
ies :: (Env, Store) -> Ident -> TValue -> (Env, Store)
ies (env, st) id val = let loc = nextLoc st in	
			 let nst = ist st loc val in
			  let nenv = ienv env id loc in
			   (nenv, nst)

insertMany :: (Env, Store) -> [(Ident, TValue)] -> (Env, Store)
insertMany = foldl (\es (i,v) -> ies es i v)


buildType :: Type -> [Sign] -> Type
buildType t signs = case signs of
	[] -> t
	(Sign h _):tl -> TFun h (buildType t tl)


