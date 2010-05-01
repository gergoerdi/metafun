module Metafun.Compiler.State where

import Control.Monad.State    
import Control.Monad
import qualified Language.CxxMPL.Syntax as MPL
import qualified Language.Kiff.Syntax as Kiff
    
data CompilerState = CSt { unique :: Int }
type Compile a = State CompilerState a
                       
mkCompilerState = CSt { unique = 0 }    
    
concatMapM f xs = liftM concat (mapM f xs)

newMetaVarName :: Compile MPL.MetaVarName
newMetaVarName = do st <- get
                    let u = unique st
                        u' = u + 1
                        st' = st{unique = u'}
                    put st'
                    return $ "_p" ++ (show u)
