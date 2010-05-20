module Language.Kiff.Typing.Unify (unify, fitDecl) where

import Language.Kiff.Syntax
import Language.Kiff.Typing
import Language.Kiff.Typing.Errors
import Language.Kiff.Typing.Substitution
    
data Unification  = Skip
                  | Incongruent
                  | Flip Unification
                  | OccursFailed
                  | Substitute Tv Ty
                  | Recurse [TyEq]

unifyEq :: TyEq -> Unification
unifyEq (TyPrimitive p :=: TyPrimitive p')               = if p == p' then Skip else Incongruent
unifyEq (TyData d      :=: TyData d')                    = if d == d' then Skip else Incongruent
unifyEq (TyVar v       :=: TyVar v')       | v == v'     = Skip
unifyEq (TyVar v       :=: t')             | occurs v t' = OccursFailed
                                           | otherwise   = Substitute v t'
unifyEq (t             :=: TyVar v)                      = Flip Incongruent
unifyEq (TyFun t u     :=: TyFun t' u')                  = Recurse [t :=: t', u :=: u']
unifyEq (TyApp t u     :=: TyApp t' u')                  = Recurse [t :=: t', u :=: u']
unifyEq (TyList t      :=: TyList t')                    = Recurse [t :=: t']
unifyEq _                                                = Incongruent

unify' :: Bool -> [TyEq] -> Either TypingError Subst
unify' _         []                   = Right $ empty
unify' leftOnly  (eq@(t :=: t'):eqs)  = process $ unifyEq eq
    where  process Skip              = unify' leftOnly eqs
           process (Recurse eqs')    = unify' leftOnly (eqs' ++ eqs)
           process Incongruent       = Left $ Unsolvable t t'
           process (Flip u)          = process $ if leftOnly then u else unifyEq (t' :=: t)
           process OccursFailed      = Left $ InfiniteType t t'
           process (Substitute x t)  = do s <- unify' leftOnly eqs'
                                          return $ add s x t
               where eqs' = map (\ (t :=: t') -> (subst s t) :=: (subst s t')) eqs
                         where s = add empty x t

unify :: [TyEq] -> Either TypingError Subst                                    
unify = unify' False

fitDecl :: Ty -> Ty -> Either TypingError Subst                                    
fitDecl tyDecl ty = unify' True [ty :=: tyDecl]
