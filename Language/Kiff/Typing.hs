{-# LANGUAGE TypeSynonymInstances #-}

module Language.Kiff.Typing where

import Language.Kiff.Syntax
    
import qualified Data.Map as Map
import Data.Supply

data TyEq = Ty :=: Ty deriving Show
          
class Typed a where
    getTy :: a -> Ty
    mapTy :: (Ty -> Ty) -> a -> a

instance Typed TDef where
    getTy (Def tau name decl tdefeqs) = tau
    mapTy f (Def tau name decl defeqs) = Def tau' name decl defeqs'
        where tau' = f tau
              defeqs' = map (mapTy f) defeqs

instance Typed TDefEq where
    getTy (DefEq tau pats body) = tau
    mapTy f (DefEq tau pats body) = DefEq tau' pats' body'
        where tau' = f tau
              pats' = map (mapTy f) pats
              body' = mapTy f body

instance Typed TPat where
    getTy (PVar tau _)    = tau
    getTy (PApp tau _ _)  = tau
    getTy (Wildcard tau)  = tau
    getTy (IntPat tau _)  = tau
    getTy (BoolPat tau _) = tau

    mapTy f (PVar tau var)       = PVar (f tau) var
    mapTy f (PApp tau con pats)  = PApp (f tau) con (map (mapTy f) pats)
    mapTy f (Wildcard tau)       = Wildcard (f tau)
    mapTy f (IntPat tau n)       = IntPat (f tau) n
    mapTy f (BoolPat tau b)      = BoolPat (f tau) b

instance Typed TExpr where
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
                                   
    mapTy f (Var tau var)                  = Var (f tau) var
    mapTy f (Con tau con)                  = Con (f tau) con
    mapTy f (App tau fun x)                = App (f tau) (mapTy f fun) (mapTy f x)
    mapTy f (Lam tau pats body)            = Lam (f tau) (map (mapTy f) pats) (mapTy f body)
    mapTy f (Let tau defs body)            = Let (f tau) (map (mapTy f) defs) (mapTy f body)
    mapTy f (PrimBinOp tau op left right)  = PrimBinOp (f tau) op (mapTy f left) (mapTy f right)
    mapTy f (IfThenElse tau cond thn els)  = IfThenElse (f tau) (mapTy f cond) (mapTy f thn) (mapTy f els)
    mapTy f (IntLit tau n)                 = IntLit (f tau) n
    mapTy f (BoolLit tau b)                = BoolLit (f tau) b
    mapTy f (UnaryMinus tau expr)          = UnaryMinus (f tau) (mapTy f expr)
                                   
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
                    
