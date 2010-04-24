module Language.Kiff.Typing.Infer where

import Language.Kiff.Syntax
import Language.Kiff.Typing.Substitution
import Language.Kiff.Typing.Unify
import Language.Kiff.Typing.Instantiate

import qualified Data.Map as Map
import Data.Supply
import Control.Monad.State

data Ctx = Ctx { conmap :: Map.Map DataName Ty,
                 varmap :: Map.Map VarName (Ty, Bool)}
         deriving Show

tyFun :: [Ty] -> Ty
tyFun = foldr1 TyFun

tyApp :: [Ty] -> Ty
tyApp = foldr1 TyApp
         
mkCtx :: Supply TvId -> (Ctx, Supply TvId)
mkCtx ids = (ctx, ids')
    where ctx = Ctx { conmap = Map.fromList [("nil", tyList),
                                             ("cons", tyFun [TyVarId i, tyList, tyList])],
                      varmap = Map.empty }
          i = supplyValue ids''
          tyList = TyApp (TyData "list") (TyVarId i)
          (ids', ids'') = split2 ids

collectExpr :: Ctx -> Supply TvId -> Expr -> (Ty, [TyEq])
collectExpr ctx ids (Var var) = case Map.lookup var (varmap ctx) of
                                  Just (tau, poly) -> (if poly then instantiate ids tau else tau, [])
collectExpr ctx ids (Con con) = case Map.lookup con (conmap ctx) of
                                  Just t -> (instantiate ids t, [])
collectExpr ctx ids (App f x) = let (ids1, ids2) = split2 ids
                                    (ft, feqs) = collectExpr ctx ids1 f
                                    (xt, xeqs) = collectExpr ctx ids2 x
                                    tau = TyVarId $ supplyValue ids
                                in (tau, (ft :=: TyFun xt tau):(feqs ++ xeqs))
collectExpr ctx ids (PrimBinOp op left right) = let  (ids1, ids2) = split2 ids
                                                     (lt, leqs) = collectExpr ctx ids1 left
                                                     (rt, reqs) = collectExpr ctx ids2 right
                                                     (t1, t2, t3) = typeOfOp op
                                                     tau = tyFun  [TyPrimitive t1, TyPrimitive t2, TyPrimitive t3]
                                                     alpha = TyVarId $ supplyValue ids
                                                     tau' = tyFun [lt, rt, alpha]
                                                in (alpha, (tau :=: tau'):(leqs ++ reqs))
collectExpr ctx ids (IfThenElse cond thn els) = let  (ids1, ids2, ids3) = split3 ids
                                                     (ct, ceqs) = collectExpr ctx ids1 cond
                                                     (tt, teqs) = collectExpr ctx ids2 thn
                                                     (et, eeqs) = collectExpr ctx ids3 els
                                                     alpha = TyVarId $ supplyValue ids
                                                in (alpha, (tt :=: alpha):(et :=: alpha):(ct :=: TyPrimitive TyBool):(ceqs++teqs++eeqs))
collectExpr ctx ids (IntLit _) = (TyPrimitive TyInt, [])
collectExpr ctx ids (BoolLit _) = (TyPrimitive TyBool, [])
collectExpr ctx ids (UnaryMinus e) = let  (tau, eqs) = collectExpr ctx ids e
                                          tyInt = TyPrimitive TyBool
                                     in (tyInt, (tyInt :=: tau):eqs)
collectExpr ctx ids (Lam pats body) = let  (ids1, ids2) = split2 ids
                                           (ctx', pts, peqs) = undefined $ collectPats ctx ids1 pats
                                           (tau, eqs) = collectExpr ctx' ids2 body
                                      in (tyFun (pts ++ [tau]), peqs ++ eqs)

collectPats :: Ctx -> Supply TvId -> [Pat] -> ([Ty], [(VarName, Ty)], [TyEq])
collectPats ctx ids pats = undefined

collectPat :: Ctx -> Supply TvId -> Pat -> (Ty, [(VarName, Ty)], [TyEq])
collectPat ctx ids Wildcard         = (TyVarId $ supplyValue ids, [], [])
collectPat ctx ids (IntPat _)       = (TyPrimitive TyInt, [], [])
collectPat ctx ids (BoolPat _)      = (TyPrimitive TyBool, [], [])
collectPat ctx ids (PVar var)       = let tau = TyVarId $ supplyValue ids
                                      in (tau, [], [])
collectPat ctx ids (PApp con pats)  = let  (ids', ids'') = split2 ids
                                           t = case Map.lookup con (conmap ctx) of
                                                 Just t -> instantiate ids' t
                                           (ts, binds, eqs) = collectPats ctx ids'' pats
                                           alpha = TyVarId $ supplyValue ids
                                      in (alpha, binds, (t :=: tyFun (ts ++ [alpha])):eqs)
intOp = (TyInt, TyInt, TyInt)                                                                        
intRel = (TyInt, TyInt, TyBool)
boolOp = (TyBool, TyBool, TyBool)
        
typeOfOp OpAdd = intOp
typeOfOp OpSub = intOp
