{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Metafun.Compiler.State
    (Compiler, runCompiler,
     newVar,
     getScopeVars, withScopeVars,
     lookupLiftedName, withLiftedNames,
     output) where

import Control.Monad.RWS
import Control.Monad
import Control.Applicative
import qualified Language.CxxMPL.Syntax as MPL
import qualified Language.Kiff.Syntax as Kiff
import qualified Data.Map as Map

type LiftMap = Map.Map Kiff.VarName MPL.Name    
    
newtype Serial = Serial Integer deriving Show
data Scope = Scope { liftedMap :: Map.Map Kiff.VarName MPL.Name,
                     scope :: [MPL.MetaVarDecl] }

newtype Compiler a = Compiler { unCompiler :: RWS Scope [MPL.Def] Serial a} deriving (Functor, Applicative, Monad)

runCompiler :: Compiler () -> MPL.Program
runCompiler (Compiler rws) = let (_, state, output) = (runRWS rws) (Scope Map.empty []) (Serial 0)
                             in MPL.Program output

output :: MPL.Def -> Compiler ()
output def = Compiler $ tell [def]
                             
fresh :: Compiler Integer
fresh = Compiler $ do Serial i <- get
                      put (Serial (succ i))
                      return i

newVar :: Compiler MPL.Name
newVar = do i <- fresh
            return $ "_v" ++ (show i)

withScopeVars :: [MPL.MetaVarDecl] -> Compiler a -> Compiler a
withScopeVars vars = Compiler . local addVars . unCompiler
    where addVars s@Scope{scope = scope} = s{scope = vars ++ scope}

getScopeVars :: Compiler [MPL.MetaVarDecl]
getScopeVars = Compiler $ do Scope{scope = scope} <- ask
                             return scope

getLiftedMap :: Compiler (Map.Map Kiff.VarName MPL.Name)
getLiftedMap = Compiler $ liftM liftedMap ask
                                    
lookupLiftedName :: Kiff.VarName -> Compiler (Maybe MPL.Name)
lookupLiftedName v = do liftedMap <- getLiftedMap
                        return $ Map.lookup v liftedMap
                                    
mkLiftedName :: Kiff.VarName -> Compiler MPL.Name
mkLiftedName v = do i <- fresh
                    return $ "_" ++ v ++ "_" ++ (show i)

withLiftedNames :: [Kiff.VarName] -> Compiler a -> Compiler a
withLiftedNames vs compiler = do vs' <- mapM mkLiftedName vs
                                 Compiler $ local (addLifted (zip vs vs')) (unCompiler compiler)
    where addLifted newLifts s@Scope{liftedMap = liftedMap} = s{liftedMap = Map.union liftedMap $ Map.fromList newLifts}
                                           
