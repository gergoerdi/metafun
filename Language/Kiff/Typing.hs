module Language.Kiff.Typing where

import Language.Kiff.Syntax
    
import qualified Data.Map as Map
import Data.Supply

data TyEq = Ty :=: Ty deriving Show
          
class Typed a where
    getTy :: a -> Ty
    mapTy :: (Ty -> Ty) -> a -> a

instance Typed TDef where
    getTy (TDef tau name decl tdefeqs) = tau
    mapTy f (TDef tau name decl tdefeqs) = TDef tau' name decl tdefeqs'
        where tau' = f tau
              tdefeqs' = map (mapTy f) tdefeqs

instance Typed TDefEq where
    getTy (TDefEq tau tpats tbody) = tau
    mapTy f (TDefEq tau tpats tbody) = TDefEq tau' tpats' tbody'
        where tau' = f tau
              tpats' = map (mapTy f) tpats
              tbody' = mapTy f tbody

instance Typed TPat where
    getTy (TPVar tau _)    = tau
    getTy (TPApp tau _ _)  = tau
    getTy (TWildcard tau)  = tau
    getTy (TIntPat _)    = TyPrimitive TyInt
    getTy (TBoolPat _)   = TyPrimitive TyBool

    mapTy f (TPVar tau var)        = TPVar (f tau) var
    mapTy f (TPApp tau con tpats)  = TPApp (f tau) con (map (mapTy f) tpats)
    mapTy f (TWildcard tau)        = TWildcard (f tau)
    mapTy f (TIntPat n)          = TIntPat n
    mapTy f (TBoolPat b)         = TBoolPat b

instance Typed TExpr where
    getTy (TVar tau _)             = tau
    getTy (TCon tau _)             = tau
    getTy (TApp tau _ _)           = tau
    getTy (TLam tau _ _)           = tau
    getTy (TLet tau _ _)           = tau
    getTy (TPrimBinOp tau _ _ _)   = tau
    getTy (TIfThenElse tau _ _ _)  = tau
    getTy (TIntLit _)            = TyPrimitive TyInt
    getTy (TBoolLit _)           = TyPrimitive TyBool
    getTy (TUnaryMinus _)        = TyPrimitive TyInt
                                   
    mapTy f (TVar tau var)                  = TVar (f tau) var
    mapTy f (TCon tau con)                  = TCon (f tau) con
    mapTy f (TApp tau fun x)                = TApp (f tau) (mapTy f fun) (mapTy f x)
    mapTy f (TLam tau pats body)            = TLam (f tau) (map (mapTy f) pats) (mapTy f body)
    mapTy f (TLet tau defs body)            = TLet (f tau) (map (mapTy f) defs) (mapTy f body)
    mapTy f (TPrimBinOp tau op left right)  = TPrimBinOp (f tau) op (mapTy f left) (mapTy f right)
    mapTy f (TIfThenElse tau cond thn els)  = TIfThenElse (f tau) (mapTy f cond) (mapTy f thn) (mapTy f els)
    mapTy f (TIntLit n)                   = TIntLit n
    mapTy f (TBoolLit b)                  = TBoolLit b
    mapTy f (TUnaryMinus expr)            = TUnaryMinus (mapTy f expr)
                                   
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
                    
