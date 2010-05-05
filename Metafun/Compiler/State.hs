module Metafun.Compiler.State
    (Compile, mkCompilerState, concatMapM,
     newMetaVarName,
     getLiftedDefs, mkLiftedNames, withLiftedNames, addLiftedDef)
    where

import Control.Monad.State    
import Control.Monad
import qualified Language.CxxMPL.Syntax as MPL
import qualified Language.Kiff.Syntax as Kiff
import qualified Data.Map as Map
    
data CompilerState = CSt {
      unique :: Int,
      currentSpec :: MPL.MetaSpecialization,
      liftedDefs :: [(MPL.MetaDecl, [MPL.MetaDef])],
      liftedDefMap :: Map.Map Kiff.VarName MPL.MetaVarName
    }
type Compile a = State CompilerState a

mkCompilerState = CSt { unique = 0,
                        currentSpec = Nothing,
                        liftedDefs = [],
                        liftedDefMap = Map.empty
                      }    
    
concatMapM f xs = liftM concat (mapM f xs)

newMetaVarName :: Compile MPL.MetaVarName
newMetaVarName = do st <- get
                    let u = unique st
                        u' = u + 1
                        st' = st{unique = u'}
                    put st'
                    return $ "_p" ++ (show u)                           
                           
getLiftedDefs :: Compile [(MPL.MetaDecl, [MPL.MetaDef])]
getLiftedDefs = liftM liftedDefs get
    
mkLiftedNames :: [Kiff.Def Kiff.Ty] -> Compile [(MPL.MetaVarName, Kiff.Def Kiff.Ty)]
mkLiftedNames = mapM mkName
    where mkName def@(Kiff.Def _ name _ _) = do name' <- newMetaVarName
                                                return ("_" ++ name ++ name', def)

withLiftedNames :: [(MPL.MetaVarName, Kiff.Def Kiff.Ty)] -> Compile a -> Compile a
withLiftedNames namedDefs compile = do st <- get
                                       put st{liftedDefMap = liftedDefMap st `Map.union` Map.fromList (map toKV namedDefs)}
                                       result <- compile
                                       st'' <- get
                                       put st''{liftedDefMap = liftedDefMap st}
                                       return result
    where toKV (liftedName, Kiff.Def _ name _ _) = (liftedName, name)

addLiftedDef compiledDef = do st <- get
                              put st{liftedDefs = compiledDef:(liftedDefs st)}  
