module Language.Kiff.Typing.Instantiate (instantiate) where

import Language.Kiff.Syntax

import qualified Data.Map as Map
import Data.Supply
import Control.Monad.State

data St = St { idmap :: Map.Map TvId TvId,
               varmap :: Map.Map TvName TvId,
               ids :: Supply TvId}        

mkSt :: Supply TvId -> St
mkSt ids = St { idmap = Map.empty,
                varmap = Map.empty,
                ids = ids }

type Inst a = State St a

mkId :: Inst TvId
mkId = do st@St{ids = ids} <- get
          let (ids', ids'') = split2 ids
          put st{ids = ids'}
          return $ supplyValue ids''

getVar :: TvName -> Inst (Maybe TvId)
getVar v = do varmap <- liftM varmap get
              return $ Map.lookup v varmap

addVar :: TvName -> Inst TvId
addVar v = do st@St{ varmap = varmap, ids = ids} <- get
              id' <- mkId
              put st{varmap = Map.insert v id' varmap}
              return id'

ensureVar :: TvName -> Inst TvId
ensureVar v = do lookup <- getVar v
                 case lookup of
                   Just id' -> return id'
                   Nothing -> addVar v


getId :: TvId -> Inst (Maybe TvId)
getId id = do idmap <- liftM idmap get
              return $ Map.lookup id idmap
                     
addId :: TvId -> Inst TvId
addId id = do st@St{ idmap = idmap, ids = ids} <- get
              id' <- mkId              
              put st{idmap = Map.insert id' id idmap}
              return id'

ensureId :: TvId -> Inst TvId
ensureId id = do lookup <- getId id
                 case lookup of
                   Just id' -> return id'
                   Nothing -> addId id

                              
instantiateM :: Ty -> Inst Ty
instantiateM (TyVar v)     = liftM TyVarId $ ensureVar v
instantiateM (TyVarId id)  = liftM TyVarId $ ensureId id
instantiateM (TyFun t u)   = do t' <- instantiateM t
                                u' <- instantiateM u
                                return $ TyFun t' u'
instantiateM (TyApp t u)   = do t' <- instantiateM t
                                u' <- instantiateM u
                                return $ TyApp t' u'
instantiateM ty            = return ty
           
instantiate :: Supply TvId -> Ty -> Ty
instantiate ids ty = evalState (instantiateM ty) (mkSt ids)
