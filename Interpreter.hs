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

ienv :: Env -> Ident -> Value -> Env
ienv env id val = Map.insert id val env

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

insertMatch :: Env -> [(Ident, Value)] -> Env
insertMatch = foldl (\env (i,v) -> ienv env i v)

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

match :: Value -> Match -> (Bool,[(Ident,Value)])
match v m = case m of
	MatchEmpList -> case v of 
		VList [] -> (True, [])
		_ -> (False,[])
	MatchHeadTail idH idT -> case v of
		VList (h:t) -> (True, [(idH,h), (idT,VList t)])
		_ -> (False,[])

matchCase :: Env -> Value -> [Patt] -> Err Value
matchCase env v patts = case patts of
	[] -> fail "match not found"
	[(Pattern m e)] -> let (good, matchs) = match v m in
		if good then let nenv = insertMatch env matchs in eval nenv e 
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










