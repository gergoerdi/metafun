module Language.Kiff.Typing where

import Language.Kiff.Syntax
    
import qualified Data.Map as Map
import Data.Supply

data TyEq = Ty :=: Ty deriving Show
          
data VarBind  = Mono Ty
              | Poly Ty
              deriving Show
    
data Ctx = Ctx { conmap :: Map.Map DataName Ty,
                 varmap :: Map.Map VarName VarBind
               }
           deriving Show

lookupVar :: Ctx -> VarName -> Maybe VarBind
lookupVar ctx v = Map.lookup v (varmap ctx)
                    
tyFun :: [Ty] -> Ty
tyFun = foldr1 TyFun

tyApp :: [Ty] -> Ty
tyApp = foldr1 TyApp
         
mkCtx :: Supply TvId -> Ctx
mkCtx ids = Ctx { conmap = Map.fromList [("nil", tyList),
                                         ("cons", tyFun [TyVar (TvId i), tyList, tyList])],
                  varmap = Map.fromList [("not",  (Poly $ tyFun [TyPrimitive TyBool, TyPrimitive TyBool]))]
                }
    where i = supplyValue ids
          tyList = TyList (TyVar (TvId i))

addVar :: Ctx -> (VarName, VarBind) -> Ctx
addVar ctx@Ctx{varmap = varmap} (name, bind) = ctx{varmap = varmap'}
    where varmap' = Map.insert name bind varmap
                   
addMonoVar :: Ctx -> (VarName, Ty) -> Ctx
addMonoVar ctx (name, ty) = addVar ctx (name, (Mono ty))

addPolyVar :: Ctx -> (VarName, Ty) -> Ctx
addPolyVar ctx (name, ty) = addVar ctx (name, (Poly ty))

occurs :: Tv -> Ty -> Bool
occurs v (TyVar v')   = v == v'
occurs v (TyFun t u)  = occurs v t || occurs v u
occurs v (TyApp t u)  = occurs v t || occurs v u
occurs v (TyList t)   = occurs v t
occurs v _            = False                         
                    
