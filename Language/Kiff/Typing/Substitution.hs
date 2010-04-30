module Language.Kiff.Typing.Substitution (Subst, empty, add, xform) where

import Language.Kiff.Syntax
import qualified Data.Map as Map
    
newtype Subst = Subst (Map.Map Tv Ty) deriving Show

empty = Subst (Map.empty)

add :: Subst -> Tv -> Ty -> Subst
add (Subst m) v t = Subst $ Map.insert v t m

                      
xform :: Subst -> Ty -> Ty
xform s t@(TyData _)       = t
xform s t@(TyPrimitive _)  = t
xform s (TyFun t u)        = TyFun (xform s t) (xform s u)
xform s (TyApp t u)        = TyApp (xform s t) (xform s u)
xform s (TyList t)         = TyList (xform s t)
xform s (TyVar v)          = case Map.lookup v m of
                               Nothing -> TyVar v
                               Just t  -> xform s t
    where Subst m = s
