{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Kiff.Typing where

import Language.Kiff.Syntax

data TyEq = Ty :=: Ty deriving Show
          
occurs :: Tv -> Ty -> Bool
occurs v (TyVar v')   = v == v'
occurs v (TyFun t u)  = occurs v t || occurs v u
occurs v (TyApp t u)  = occurs v t || occurs v u
occurs v (TyList t)   = occurs v t
occurs v _            = False                         
                    
tyFun :: [Ty] -> Ty
tyFun = foldr1 TyFun

tyApp :: [Ty] -> Ty
tyApp = foldr1 TyApp
         
