module Language.Kiff.Typing.Substitution (Subst, empty, add, subst) where

import Language.Kiff.Syntax
import qualified Data.Map as Map
    
newtype Subst = Subst (Map.Map Tv Ty) deriving Show

empty = Subst (Map.empty)

add :: Subst -> Tv -> Ty -> Subst
add (Subst m) v t = Subst $ Map.insert v t m

                      
subst :: Subst -> Ty -> Ty
subst s t@(TyData _)       = t
subst s t@(TyPrimitive _)  = t
subst s (TyFun t u)        = TyFun (subst s t) (subst s u)
subst s (TyApp t u)        = TyApp (subst s t) (subst s u)
subst s (TyList t)         = TyList (subst s t)
subst s (TyVar v)          = case Map.lookup v m of
                               Nothing -> TyVar v
                               Just t  -> subst s t
    where Subst m = s
