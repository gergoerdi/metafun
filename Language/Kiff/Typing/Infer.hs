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
         
mkCtx :: Supply TvId -> (Ctx, Supply TvId)
mkCtx ids = (ctx, ids')
    where ctx = Ctx { conmap = Map.fromList [("nil", tyList),
                                             ("cons", tyFun [TyVarId i, tyList, tyList])],
                      varmap = Map.empty }
          i = supplyValue ids''
          tyList = TyApp (TyData "list") (TyVarId i)
          (ids', ids'') = split2 ids

collectExpr :: Ctx -> Supply TvId -> Expr -> (Ty, [TyEq])
collectExpr ctx ids (Con con) = case Map.lookup con (conmap ctx) of
                                  Just t -> (instantiate ids t, [])
collectExpr ctx ids (Var var) = case Map.lookup var (varmap ctx) of
                                  Just (t, poly) -> (if poly then instantiate ids t else t, [])
collectExpr ctx ids (App f x) = let (ids1, ids2) = split2 ids
                                    (tf, ef) = collectExpr ctx ids1 f
                                    (tx, ex) = collectExpr ctx ids2 x
                                    alpha = TyVarId $ supplyValue ids
                                in (alpha, (tf :=: TyFun tx alpha):(ef ++ ex))
collectExpr ctx ids (PrimBinOp op left right) = let (ids1, ids2) = split2 ids
                                                    (tleft, eleft)    = collectExpr ctx ids1 left
                                                    (tright, eright)  = collectExpr ctx ids2 right
                                                    (t1, t2, t3) = typeOfOp op
                                                    t = tyFun  [TyPrimitive t1, TyPrimitive t2, TyPrimitive t3]
                                                    alpha = TyVarId $ supplyValue ids
                                                    t' = tyFun [tleft, tright, alpha]
                                                in (alpha, (t :=: t'):(eleft ++ eright))
collectExpr ctx ids (IntLit _) = (TyPrimitive TyInt, [])
collectExpr ctx ids (BoolLit _) = (TyPrimitive TyBool, [])
                                   

intOp = (TyInt, TyInt, TyInt)                                                                        
intRel = (TyInt, TyInt, TyBool)
boolOp = (TyBool, TyBool, TyBool)
        
typeOfOp OpAdd = intOp
typeOfOp OpSub = intOp
