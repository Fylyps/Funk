module TypeChecker where

import AbsFunk
import ErrM
import Model
import FunkStdLib
import qualified Data.Map as Map  


checkDec :: (Env, Store) -> Sign -> [Sign] -> Exp -> Err (Env, Store)
checkDec es (Sign t id) args exp = let nes = insertSigns es args in 
				   let withme = ies nes id (buildType t args, VNone) in do
		check withme t exp
		return withme	

checkDecs :: (Env, Store) -> [Decl] -> Err (Env, Store)
checkDecs es decs = case decs of
	[] -> return es
	[(Declaration sign args exp)] -> checkDec es sign args exp
	h:t -> do
		nes <- checkDecs es [h]
		checkDecs nes t

checkTypes :: Prog -> Err Bool  
checkTypes (Program decs t exp) = let std = insertStd (Map.empty, Map.empty) in do
	es <- checkDecs std decs
	check es t exp

checkApplication :: (Env, Store) -> Type -> Type -> [Exp] -> Err Bool
checkApplication es target tfun args = case args of
	[] -> if target /= tfun then fail $ "function result different" else return True
	h:t -> case tfun of
		TFun argT resT -> do
			checkApplication es target resT t
			check es argT h
		_ -> fail $ "too many arguments" ++ (show tfun)++(show target)

checkAll :: (Env, Store) -> Type -> [Exp] -> Err Bool
checkAll es t exps = case exps of
	[] -> return True
	h:tl -> do
		check es t h
		checkAll es t tl

insertSigns :: (Env, Store) -> [Sign] -> (Env, Store)
insertSigns es signs = case signs of
	[] -> es
	(Sign st sid):t -> insertSigns (ies es sid (st, VNone)) t

checkAllSim :: (Env, Store) -> [Type] -> [Exp] -> Err Bool
checkAllSim es [] [] = return True
checkAllSim es [] _ = fail "wrong tuple length"
checkAllSim es _ [] = fail "wrong tuple length"
checkAllSim es (h:t) (e:exps) = do
	check es h e
	checkAllSim es t exps

check :: (Env, Store) -> Type -> Exp -> Err Bool
check es@(env, st) t e = case e of
	EGet id -> do
		(tv, _) <- lookFor es id 
		if tv == t then return True else fail "type mismatch"
	EApplication id params -> do
		(tf, _) <- lookFor es id
		checkApplication es t tf params
	ELet d exp -> do
		nes <- checkDecs es [d]
		check nes t exp
	ECase exp patts -> let exps = map (\(Pattern _ e) -> e) patts in do
		checkAll es t exps
	ELambda tlam signs exp -> let nes = insertSigns es signs in do
		check nes tlam exp
		let tl = buildType tlam signs in
			if tl == t then return True else fail "lambda with different type"
	EIf cond e1 e2 -> do
		check es TBool cond
		check es t e1
		check es t e2
	EList exps -> case t of
		TList tl -> checkAll es tl exps
		_ -> fail "type mismatch (got list)"
	EHeadTail exph expt -> case t of
		TList tl -> do
			check es tl exph
			check es (TList tl) expt
		_ -> fail "type mismatch (got list join)"
	EEmptyList et -> if t /= TList et then fail "type mismatch (got empty list)" else return True
	EEmptyTuple -> if t /= TTuple [] then fail "type mismatch (got empty tuple)" else return True
	ETuple exps -> case t of
		TTuple lt -> checkAllSim es lt exps
	EInt n -> if t==TInt then return True else fail "type mismatch (got int)"
	EFalse -> if t==TBool then return True else fail "type mismatch (got bool)"
	ETrue -> if t==TBool then return True else fail "type mismatch (got bool)"
	EPom e -> check es t e






