{-# LANGUAGE FlexibleInstances #-}

module Language.Kiff.Typing where

import Language.Kiff.Syntax
    
import qualified Data.Map as Map
import Data.Supply

data TyEq = Ty :=: Ty deriving Show
          
class Typed a where
    getTy :: a -> Ty

instance Typed (Def Ty) where
    getTy (Def tau name decl tdefeqs) = tau

instance Typed (DefEq Ty) where
    getTy (DefEq tau pats body) = tau

instance Typed (Pat Ty) where
    getTy (PVar tau _)    = tau
    getTy (PApp tau _ _)  = tau
    getTy (Wildcard tau)  = tau
    getTy (IntPat tau _)  = tau
    getTy (BoolPat tau _) = tau

instance Typed (Expr Ty) where
    getTy (Var tau _)             = tau
    getTy (Con tau _)             = tau
    getTy (App tau _ _)           = tau
    getTy (Lam tau _ _)           = tau
    getTy (Let tau _ _)           = tau
    getTy (PrimBinOp tau _ _ _)   = tau
    getTy (IfThenElse tau _ _ _)  = tau
    getTy (IntLit tau _)          = tau
    getTy (BoolLit tau _)         = tau
    getTy (UnaryMinus tau _)      = tau
    getTy (Not tau expr)          = tau
                                   
data VarBind  = Mono Ty
              | Poly Ty
              deriving Show
    
data Ctx = Ctx { conmap :: Map.Map DataName Ty,
                 varmap :: Map.Map VarName VarBind
               }
           deriving Show

lookupVar :: Ctx -> VarName -> Maybe VarBind
lookupVar ctx v = Map.lookup v (varmap ctx)
                    
tyFun :: [Ty] -> Ty
tyFun = foldr1 TyFun

tyApp :: [Ty] -> Ty
tyApp = foldr1 TyApp
         
mkCtx :: Supply TvId -> Ctx
mkCtx ids = Ctx { conmap = Map.fromList [("nil", tyList),
                                         ("cons", tyFun [TyVar (TvId i), tyList, tyList])],
                  varmap = Map.fromList [("not",  (Poly $ tyFun [TyPrimitive TyBool, TyPrimitive TyBool]))]
                }
    where i = supplyValue ids
          tyList = TyList (TyVar (TvId i))

addVar :: Ctx -> (VarName, VarBind) -> Ctx
addVar ctx@Ctx{varmap = varmap} (name, bind) = ctx{varmap = varmap'}
    where varmap' = Map.insert name bind varmap
                   
addMonoVar :: Ctx -> (VarName, Ty) -> Ctx
addMonoVar ctx (name, ty) = addVar ctx (name, (Mono ty))

addPolyVar :: Ctx -> (VarName, Ty) -> Ctx
addPolyVar ctx (name, ty) = addVar ctx (name, (Poly ty))

occurs :: Tv -> Ty -> Bool
occurs v (TyVar v')   = v == v'
occurs v (TyFun t u)  = occurs v t || occurs v u
occurs v (TyApp t u)  = occurs v t || occurs v u
occurs v _            = False                         
                    
