module Interpreter where

import AbsFunk
import ErrM
import Model
import FunkStdLib
import qualified Data.Map as Map  

buildValue :: Env -> [Ident] -> Exp -> Err Value
buildValue env args exp = case args of						
	[] -> eval env exp
	h:t -> return $ VFun (\v -> buildValue (ienv env h v) t exp)

insertDec :: Env -> [Decl] -> Err Env
insertDec env decs = case decs of
	[] -> return env
	[(Declaration id args exp)] -> let val = buildValue env args exp in
		case val of
			Ok v -> return $ ienv env id v
			Bad e -> fail e
	h:t -> do 
		nenv <- insertDec env [h]
		insertDec nenv t

run :: Prog -> Err Value
run (Program decs exp) = let std = insertStd Map.empty in do
	env <- insertDec std decs
	eval env exp 

apply :: Value -> [Err Value] -> Err Value
apply f args = case f of 
	VFun inFun -> 
		case args of
			[] -> fail "too less arguments"
			[h] -> case h of
				Ok v -> inFun v 
				Bad e -> fail e
			h:t -> do
				nf <- apply f [h]
				apply nf t
	_ -> fail "applied sth to not-function"

match :: Env -> Value -> Match -> Err (Bool,[(Ident,Value)])
match env v m = case m of
	MatchHeadTail idH idT -> case v of
		VList (h:t) -> return (True, [(idH,h), (idT,VList t)])
		_ -> return (False,[])
	MatchExp exp -> do
		v2 <- eval env exp
		if v == v2 then return (True, []) else return (False, [])
	
matchCase :: Env -> Value -> [Patt] -> Err Value
matchCase env v patts = case patts of
	[] -> fail "match not found"
	[(Pattern m e)] -> do
		(good, matchs) <- match env v m
		if good then let nenv = insertMany env matchs in eval nenv e 
			else matchCase env v []
	h:t -> let r = matchCase env v [h] in case r of
			Ok v -> r
			_ -> matchCase env v t

eval :: Env -> Exp -> Err Value
eval env e = case e of
	EGet id -> lookFor env id 
	EApplication id params -> do
		f <- lookFor env id
		apply f $ map (eval env) params
	ELet d exp -> do
		nenv <- insertDec env [d]
		eval nenv exp
	ECase exp patts -> do
		v <- eval env exp
		matchCase env v patts
	EIf cond e1 e2 -> do
		v <- eval env cond
		case v of
			(VBool b) -> if b then eval env e1 else eval env e2
			_ -> fail "Not boolean value in if"
	EList exps -> case exps of
			[] -> return $ VList []
			h:t -> do
				VList nt <- eval env (EList t)
				nh <- eval env h
				return $ VList (nh:nt)
	EEmptyList -> return $ VList []			
	EEmptyTuple -> return $ VTuple Nothing Nothing
	ETuple exps -> case exps of
		[] -> return $ VTuple Nothing Nothing
		h:t -> do
			rt <- eval env (ETuple t)
			rh <- eval env h
			return $ VTuple (Just rh) (Just rt)
	EInt n -> return $ VInt n
	EFalse -> return $ VBool False
	ETrue -> return $ VBool True
	EPom e -> eval env e









