{-# LANGUAGE FlexibleContexts #-}
module Language.Kiff.Typing.Instantiate (instantiate) where

import Language.Kiff.Syntax
import Language.Kiff.Typing
import Language.Kiff.Typing.State
    
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.State

type TvMap = Map.Map Tv Tv
              
type Instantiate = StateT TvMap Typing
    
getTv :: Tv -> Instantiate (Maybe Tv)
getTv v = liftM (Map.lookup v) get

ensureTv :: Tv -> Instantiate Tv
ensureTv tv = do lookup <- getTv tv
                 case lookup of
                   Just tv' -> return tv'
                   Nothing -> inst tv
    where inst tv = do tv' <- lift mkTv
                       modify (Map.insert tv tv')
                       return tv'

instantiateM :: Ty -> Instantiate Ty
instantiateM (TyVar v)     = do mono <- lift $ isMonoVar v
                                if mono
                                   then return $ TyVar v
                                   else liftM TyVar $ ensureTv v
instantiateM (TyFun t u)   = do t' <- instantiateM t
                                u' <- instantiateM u
                                return $ TyFun t' u'
instantiateM (TyApp t u)   = do t' <- instantiateM t
                                u' <- instantiateM u
                                return $ TyApp t' u'
instantiateM (TyList t)    = do t' <- instantiateM t
                                return $ TyList t'
instantiateM ty            = return ty

instantiate :: Ty -> Typing Ty
instantiate ty = evalStateT (instantiateM ty) (Map.empty)
