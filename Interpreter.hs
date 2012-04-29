module Interpreter where

import AbsFunk
import ErrM
import Model
import FunkStdLib
import qualified Data.Map as Map  

buildValue :: (Env, Store) -> [Ident] -> Exp -> Err Value
buildValue es@(env, st) args exp = case args of						
	[] -> eval es exp
	h:t -> return $ VFun (\es2 v -> let nes = ies es2 h v in 
						(buildValue nes t exp, nes)) 

insertDec :: (Env, Store) -> Ident -> [Ident] -> Exp -> Err (Env, Store)
insertDec (env, st) id args exp = let loc = nextLoc st in
	let nenv = ienv env id loc in do 
	val <- buildValue (nenv, st) args exp
	return $ (nenv, ist st loc val)
	

insertDecs :: (Env, Store) -> [Decl] -> Err (Env, Store)
insertDecs es decs = case decs of
	[] -> return es
	[(Declaration id args exp)] -> insertDec es id args exp
	h:t -> do 
		nes <- insertDecs es [h]
		insertDecs nes t

run :: Prog -> Err Value
run (Program decs exp) = let std = insertStd (Map.empty, Map.empty) in do
	es <- insertDecs std decs
	eval es exp 

apply :: (Env, Store) -> Value -> [Err Value] -> (Err Value, (Env, Store))
apply es f args = case f of 
	VFun inFun -> 
		case args of
			[] -> (fail "too less arguments", es)
			[h] -> case h of
				Ok v -> inFun es v
				Bad e -> (fail e, es)
			h:t -> let (nf, ns) = apply es f [h] in
				case nf of 
					Ok fun -> apply ns fun t
					Bad e -> (fail e, es)
	e -> (fail $ "applied sth to not-function" ++ (show e), es)

match :: (Env, Store) -> Value -> Match -> Err (Bool,[(Ident,Value)])
match es v m = case m of
	MatchHeadTail idH idT -> case v of
		VList (h:t) -> return (True, [(idH,h), (idT,VList t)])
		_ -> return (False,[])
	MatchExp exp -> do
		v2 <- eval es exp
		if v == v2 then return (True, []) else return (False, [])
	
matchCase :: (Env, Store) -> Value -> [Patt] -> Err Value
matchCase es v patts = case patts of
	[] -> fail "match not found"
	[(Pattern m e)] -> do
		(good, matchs) <- match es v m
		if good then let nes = insertMany es matchs in eval nes e 
			else matchCase es v []
	h:t -> let r = matchCase es v [h] in case r of
			Ok v -> r
			_ -> matchCase es v t

eval :: (Env, Store) -> Exp -> Err Value
eval es@(env, st) e = case e of
	EGet id -> lookFor es id 
	EApplication id params -> do
		f <- lookFor es id
		fst $ apply es f $ map (eval es) params
	ELet d exp -> do
		nes <- insertDecs es [d]
		eval nes exp
	ECase exp patts -> do
		v <- eval es exp
		matchCase es v patts
	EIf cond e1 e2 -> do
		v <- eval es cond
		case v of
			(VBool b) -> if b then eval es e1 else eval es e2
			_ -> fail "Not boolean value in if"
	EList exps -> case exps of
			[] -> return $ VList []
			h:t -> do
				VList nt <- eval es (EList t)
				nh <- eval es h
				return $ VList (nh:nt)
	EEmptyList -> return $ VList []			
	EEmptyTuple -> return $ VTuple Nothing Nothing
	ETuple exps -> case exps of
		[] -> return $ VTuple Nothing Nothing
		h:t -> do
			rt <- eval es (ETuple t)
			rh <- eval es h
			return $ VTuple (Just rh) (Just rt)
	EInt n -> return $ VInt n
	EFalse -> return $ VBool False
	ETrue -> return $ VBool True
	EPom e -> eval es e









