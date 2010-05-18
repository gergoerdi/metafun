{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Kiff.Typing.State
    (Typing, runTyping,
     lookupVar, lookupCon,
     mkTv, mkTyVar,
     collectEqs, yieldEqs,
     isMonoVar, withMonoVars, withPolyVars, VarBind(..),
     withCons) where

import Language.Kiff.Syntax
import Language.Kiff.Typing
import Control.Monad.RWS
import qualified Data.Map as Map    
import Data.Maybe
    
data VarBind  = Mono Ty
              | Poly Ty
              deriving Show                           
                       
data Ctx = Ctx { conmap :: Map.Map DataName Ty,
                 varmap :: Map.Map VarName VarBind }
           deriving Show

newtype Typing a = Typing {rws :: RWS Ctx [TyEq] TvId a} deriving Monad

lookupVar :: VarName -> Typing (Maybe VarBind)
lookupVar v = Typing $ do varmap <- asks varmap
                          return $ Map.lookup v varmap

lookupCon :: ConName -> Typing (Maybe Ty)
lookupCon c = Typing $ do conmap <- asks conmap
                          return $ Map.lookup c conmap
                        
mkTv :: Typing Tv
mkTv = Typing $ do i <- get
                   modify succ
                   return $ TvId i

mkTyVar :: Typing Ty
mkTyVar = liftM TyVar mkTv

runTyping :: Typing a -> a
runTyping typing = let Typing rws = typing'
                       (result, _, _) = (runRWS rws) ctx 0
                   in result
    where ctx = Ctx{conmap=Map.empty, varmap=Map.empty}
          typing' = do alpha <- mkTyVar
                       let nil = ("nil", TyList alpha)
                           cons = ("cons", tyFun [alpha, TyList alpha, TyList alpha])
                       withCons [nil, cons] typing
                
-- mkCtx :: Supply TvId -> Ctx
-- mkCtx ids = Ctx { conmap = Map.fromList [("nil", tyList),
--                                          ("cons", tyFun [TyVar (TvId i), tyList, tyList])],
--                   varmap = Map.fromList [("not",  (Poly $ tyFun [TyPrimitive TyBool, TyPrimitive TyBool]))]
--                 }
--     where i = supplyValue ids
--           tyList = TyList (TyVar (TvId i))

withCons :: [(VarName, Ty)] -> Typing a -> Typing a
withCons cons = Typing . local (\ ctx@Ctx{conmap = conmap} -> ctx{conmap = conmap `Map.union` (Map.fromList cons)}) . rws

withVars :: [(VarName, VarBind)] -> Typing a -> Typing a
withVars binds = Typing . local (\ ctx@Ctx{varmap = varmap} -> ctx{varmap = varmap `Map.union` (Map.fromList binds)}) . rws

withMonoVars :: [(VarName, Ty)] -> Typing a -> Typing a
withMonoVars binds = withVars $ map (\ (name, ty) -> (name, Mono ty)) binds

withPolyVars :: [(VarName, Ty)] -> Typing a -> Typing a
withPolyVars binds = withVars $ map (\ (name, ty) -> (name, Poly ty)) binds

isMonoVar :: Tv -> Typing Bool
isMonoVar tv = Typing $ do ctx  <- ask
                           return $ any (occurs tv) (monoTys ctx)
    where monoTys ctx = mapMaybe unpack $ Map.elems (varmap ctx)
              where unpack (Poly _) = Nothing
                    unpack (Mono tau) = Just tau

collectEqs :: Typing a -> Typing (a, [TyEq])
collectEqs = Typing . listen . rws

yieldEqs :: [TyEq] -> Typing ()
yieldEqs = Typing . tell
