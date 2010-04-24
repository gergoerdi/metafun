module Language.Kiff.Typing.Substitution (Subst, empty, add, xform) where

import Language.Kiff.Syntax
import qualified Data.Map as Map
    
newtype Subst = Subst (Map.Map TvId Ty) deriving Show

empty = Subst (Map.empty)

add :: Subst -> TvId -> Ty -> Subst
add (Subst m) x t = Subst $ Map.insert x t m

xform :: Subst -> Ty -> Ty
xform s t@(TyData _)       = t
xform s t@(TyPrimitive _)  = t
xform s (TyFun t u)        = TyFun (xform s t) (xform s u)
xform s (TyApp t u)        = TyApp (xform s t) (xform s u)
xform s (TyVarId x)        = case Map.lookup x m of
                               Nothing -> TyVarId x
                               Just t  -> xform s t
    where Subst m = s
