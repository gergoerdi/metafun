module Language.Kiff.Typing.Instantiate (instantiate) where

import Language.Kiff.Syntax
import Language.Kiff.Typing

import qualified Data.Map as Map
import Data.Supply
import Control.Monad.State

data St = St { tvmap :: Map.Map Tv Tv,
               ctx :: Ctx,
               ids :: Supply TvId}        

mkSt :: Supply TvId -> Ctx -> St
mkSt ids ctx = St { tvmap = Map.empty,
                    ctx = ctx,
                    ids = ids }

type Inst a = State St a

mkId :: Inst Tv
mkId = do st@St{ids = ids} <- get
          let (ids', ids'') = split2 ids
          put st{ids = ids'}
          return $ TvId $ supplyValue ids''

getTv :: Tv -> Inst (Maybe Tv)
getTv v = do tvmap <- liftM tvmap get
             return $ Map.lookup v tvmap

instTv :: Tv -> Inst Tv
instTv v = do st@St{ tvmap = tvmap, ids = ids} <- get
              id' <- mkId
              put st{tvmap = Map.insert v id' tvmap}
              return id'

ensureTv :: Tv -> Inst Tv
ensureTv v = do lookup <- getTv v
                case lookup of
                  Just v' -> return v'
                  Nothing -> instTv v


instantiateM :: Ty -> Inst Ty
instantiateM (TyVar v)     = liftM TyVar $ ensureTv v
instantiateM (TyFun t u)   = do t' <- instantiateM t
                                u' <- instantiateM u
                                return $ TyFun t' u'
instantiateM (TyApp t u)   = do t' <- instantiateM t
                                u' <- instantiateM u
                                return $ TyApp t' u'
instantiateM (TyList t)    = do t' <- instantiateM t
                                return $ TyList t'
instantiateM ty            = return ty
           
instantiate :: Supply TvId -> Ctx -> Ty -> Ty
instantiate ids ctx ty = evalState (instantiateM ty) (mkSt ids ctx)
