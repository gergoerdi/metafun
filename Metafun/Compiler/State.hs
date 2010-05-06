module Metafun.Compiler.State
    (Compile, mkCompilerState, concatMapM,
     newMetaVarName,
     getLiftedDefs, mkLiftedNames, withLiftedNames, addLiftedDef, lookupLiftedName,
     getScopeVars, withScopeVars)
    where

import Control.Monad.State    
import Control.Monad
import qualified Language.CxxMPL.Syntax as MPL
import qualified Language.Kiff.Syntax as Kiff
import qualified Data.Map as Map

data CompilerState = CSt {
      unique :: Int,
      currentVars :: [MPL.MetaVarDecl],
      liftedDefs :: [(MPL.MetaDecl, [MPL.MetaDef])],
      liftedDefMap :: Map.Map Kiff.VarName MPL.MetaVarName
    }
type Compile a = State CompilerState a

mkCompilerState = CSt { unique = 0,
                        currentVars = [],
                        liftedDefs = [],
                        liftedDefMap = Map.empty
                      }    
    
concatMapM f xs = liftM concat (mapM f xs)

newUnique :: Compile Int
newUnique = do st <- get
               let u = unique st
                   u' = u + 1
                   st' = st{unique = u'}
               put st'
               return u
                  
newMetaVarName :: Compile MPL.MetaVarName
newMetaVarName = do u <- newUnique
                    return $ "_p" ++ (show u)
                           
getLiftedDefs :: Compile [(MPL.MetaDecl, [MPL.MetaDef])]
getLiftedDefs = liftM liftedDefs get
    
mkLiftedNames :: [Kiff.Def Kiff.Ty] -> Compile [(MPL.MetaVarName, Kiff.Def Kiff.Ty)]
mkLiftedNames = mapM mkName
    where mkName def@(Kiff.Def _ name _ _) = do u <- newUnique
                                                let name' = "_" ++ name ++ "_" ++ (show u)
                                                return (name', def)

withLiftedNames :: [(MPL.MetaVarName, Kiff.Def Kiff.Ty)] -> Compile a -> Compile a
withLiftedNames namedDefs compile = do st <- get
                                       put st{liftedDefMap = liftedDefMap st `Map.union` Map.fromList (map toKV namedDefs)}
                                       result <- compile
                                       st'' <- get
                                       put st''{liftedDefMap = liftedDefMap st}
                                       return result
    where toKV (liftedName, Kiff.Def _ name _ _) = (name, liftedName)

addLiftedDef compiledDef = do st <- get
                              put st{liftedDefs = compiledDef:(liftedDefs st)}  

lookupLiftedName :: Kiff.VarName -> Compile (Maybe MPL.MetaVarName)
lookupLiftedName varname = do st <- get                                   
                              return $ Map.lookup varname (liftedDefMap st)

getScopeVars :: Compile [MPL.MetaVarDecl]
getScopeVars = liftM currentVars get 

withScopeVars :: [MPL.MetaVarDecl] -> Compile a -> Compile a
withScopeVars vars compile = do st <- get
                                let st' = st{currentVars = vars ++ currentVars st}
                                put st'
                                result <- compile
                                st'' <- get
                                put st''{currentVars = currentVars st}
                                return result
