module Language.Kiff.Typing.Errors (UnificationError(..)) where

import Language.Kiff.Syntax
    
data UnificationError  = InfiniteType Ty Ty
                       | Unsolvable Ty Ty
                       | CantFitDecl Ty Ty
                       deriving Show
