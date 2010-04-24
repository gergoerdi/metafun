module Language.Kiff.Typing.Unify (UnificationError(..), TyEq(..), unify, checkDecl) where

import Language.Kiff.Syntax
import Language.Kiff.Typing.Substitution
    
data TyEq = Ty :=: Ty deriving Show
          
data UnificationError  = InfiniteType Ty Ty
                       | Unsolvable Ty Ty
                       | CantFitDecl Ty Ty
                       deriving Show

data Unification  = Skip
                  | Incongruent
                  | Flip Unification
                  | OccursFailed
                  | Substitute TvId Ty
                  | Recurse [TyEq]

occurs :: TvId -> Ty -> Bool
occurs x (TyVarId y)  = x == y
occurs x (TyFun t u)  = occurs x t || occurs x u
occurs x (TyApp t u)  = occurs x t || occurs x u
occurs x _            = False                         
                    
unifyEq :: Ty -> Ty -> Unification
unifyEq (TyPrimitive p)  (TyPrimitive p')               = if p == p' then Skip else Incongruent
unifyEq (TyData d)       (TyData d')                    = if d == d' then Skip else Incongruent
unifyEq (TyVarId x)      (TyVarId y)      | x == y      = Skip
unifyEq (TyVarId x)      t'               | occurs x t' = OccursFailed
                                          | otherwise   = Substitute x t'
unifyEq t                (TyVarId y)                    = Flip Incongruent
unifyEq (TyFun t u)      (TyFun t' u')                  = Recurse [t :=: t', u :=: u']
unifyEq (TyApp t u)      (TyApp t' u')                  = Recurse [t :=: t', u :=: u']
unifyEq _                _                              = Incongruent

unify' :: Bool -> [TyEq] -> Either [UnificationError] Subst
unify' _         []                = Right $ empty
unify' leftOnly  ((t :=: t'):eqs)  = process $ unifyEq t t'
    where process Skip              = unify' leftOnly eqs
          process (Recurse eqs')    = unify' leftOnly (eqs' ++ eqs)
          process Incongruent       = addError $ Unsolvable t t'
          process (Flip u)          = process $ if leftOnly then u else unifyEq t' t
          process OccursFailed      = addError $ InfiniteType t t'
          process (Substitute x t)  = case unify' leftOnly eqs' of
                                        Left es -> Left es
                                        Right s -> Right $ add s x t
              where eqs' = map (\ (t :=: t') -> (xform s t) :=: (xform s t')) eqs
                    s = add empty x t

          addError e = case unify' leftOnly eqs of
                         Left es -> Left $ e:es
                         Right _ -> Left $ [e]

unify = unify' False
checkDecl tyDecl ty = unify' True [ty :=: tyDecl]
