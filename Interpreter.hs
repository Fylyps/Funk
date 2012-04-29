module Interpreter where

import AbsFunk
import ErrM
import Model
import FunkStdLib
import qualified Data.Map as Map  

buildType :: Type -> [Sign] -> Type
buildType t signs = case signs of
	[] -> t
	(Sign h _):tl -> TFun h (buildType t tl)

buildTValue :: (Env, Store) -> Type -> [Sign] -> Exp -> Err TValue
buildTValue es@(env, st) t args exp = case args of						
	[] -> case eval es exp of
		Ok (evalt, v) -> if evalt /= t then fail "evaluated to different type" else return (t,v)
		Bad e -> fail e
	(Sign ht hid):tl -> return $ (buildType t args, VFun (\es2 arg@(targ,varg) -> if targ /= ht then (fail "runtime error: argument type mismach", es) else
						let nes = ies es2 hid arg in 
							(buildTValue nes t tl exp, nes)))

insertDec :: (Env, Store) -> Sign -> [Sign] -> Exp -> Err (Env, Store)
insertDec (env, st) (Sign t id) args exp = let loc = nextLoc st in
	let nenv = ienv env id loc in do 
	tv <- buildTValue (nenv, st) t args exp
	return $ (nenv, ist st loc tv)
	

insertDecs :: (Env, Store) -> [Decl] -> Err (Env, Store)
insertDecs es decs = case decs of
	[] -> return es
	[(Declaration sign args exp)] -> insertDec es sign args exp 
	h:t -> do 
		nes <- insertDecs es [h]
		insertDecs nes t

run :: Prog -> Err Value
run (Program decs t exp) = let std = insertStd (Map.empty, Map.empty) in do
	es <- insertDecs std decs
	(et, ev) <- eval es exp 
	if et /= t then fail "program evaluated to different type" else return ev

apply :: (Env, Store) -> TValue -> [Err TValue] -> (Err TValue, (Env, Store))
apply es f args = case f of 
	(TFun argt outt, VFun inFun) -> 
		case args of
			[] -> (fail "too less arguments", es)
			[h] -> case h of
				Ok (t,v) -> if argt /= t then (fail "runtime error: argument type mismatch", es)
						else inFun es (t,v)
				Bad e -> (fail e, es)
			h:t -> let (nf, ns) = apply es f [h] in
				case nf of 
					Ok fun -> apply ns fun t
					Bad e -> (fail e, es)
	e -> (fail $ "applied sth to not-function" ++ (show e), es)

match :: (Env, Store) -> TValue -> Match -> Err (Bool,[(Ident,TValue)])
match es (t,v) m = case m of
	MatchHeadTail idH idT -> case v of
		VList (h:tl) -> case t of
			TList inner -> return (True, [(idH,(inner, h)), (idT,(t, VList tl))])
		_ -> return (False,[])
	MatchExp exp -> do
		(t2,v2) <- eval es exp
		if t == t2 && v == v2 then return (True, []) else return (False, [])
	
matchCase :: (Env, Store) -> TValue -> [Patt] -> Err TValue
matchCase es v patts = case patts of
	[] -> fail "match not found"
	[(Pattern m e)] -> do
		(good, matchs) <- match es v m
		if good then let nes = insertMany es matchs in eval nes e 
			else matchCase es v []
	h:t -> let r = matchCase es v [h] in case r of
			Ok v -> r
			_ -> matchCase es v t

generateRestOfList :: (Env, Store) -> Type -> [Exp] -> Err [Value]
generateRestOfList es t exps = case exps of
	[] -> return []
	h:tl -> do
		(et, ev) <- eval es h
		rest <- generateRestOfList es t tl
		if et /= t then fail "elements of list have different types" else return $ ev:rest
			

eval :: (Env, Store) -> Exp -> Err TValue
eval es@(env, st) e = case e of
	EGet id -> lookFor es id 
	EApplication id params -> do
		f <- lookFor es id
		fst $ apply es f $ map (eval es) params
	ELet d exp -> do
		nes <- insertDecs es [d]
		eval nes exp
	ECase exp patts -> do
		tv <- eval es exp
		matchCase es tv patts
	ELambda t signs exp -> buildTValue es t signs exp 
	EIf cond e1 e2 -> do
		tv <- eval es cond
		case tv of
			(TBool, VBool b) -> if b then eval es e1 else eval es e2
			_ -> fail "Not boolean value in if"
	EList exps -> case exps of
			h:t -> do
				(th,nh) <- eval es h
				nt <- generateRestOfList es th t
				return $ (TList th, VList (nh:nt))
	EHeadTail exph expt -> do
			(th, vh) <- eval es exph
			(tt, vt) <- eval es expt
			case (tt, vt) of
				(TList t, VList tl) -> if th /= t then fail "cannot join to list of different type"
							else return (TList t, VList (vh:tl))
				_ -> fail "cannot join to non list element"
	EEmptyList t -> return $ (TList t, VList [])
	EEmptyTuple -> return $ (TTuple [], VTuple Nothing Nothing)
	ETuple exps -> case exps of
		[] -> eval es EEmptyTuple 
		h:t -> do
			(TTuple tt, vt) <- eval es (ETuple t)
			(th, vh) <- eval es h
			return $ (TTuple (th:tt), VTuple (Just vh) (Just vt))
	EInt n -> return $ (TInt, VInt n)
	EFalse -> return $ (TBool, VBool False)
	ETrue -> return $ (TBool, VBool True)
	EPom e -> eval es e









