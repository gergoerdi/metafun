module Language.Kiff.Typing.Errors (TypingError(..), output) where

import Language.Kiff.Syntax
import Control.Monad.Error
import Text.PrettyPrint
    
data TypingError  = InfiniteType Ty Ty
                  | Unsolvable Ty Ty
                  | CantFitDecl VarName Ty Ty
                  | MiscError String
                  deriving Show

instance (Error TypingError) where
    strMsg = MiscError             

output :: TypingError -> Doc             
output (CantFitDecl name t tDecl)  = text "Declared type" <+> text (show tDecl) <+> text "of" <+> quotes (text name) <+>
                                     text "is incongruent with inferred type" <+> text (show t)
output (InfiniteType t t')         = text "Occurs check failed for the infinite type" <+> text (show t) <+>
                                     text "=" <+> text (show t')
output (Unsolvable t t')           = text "Cannot unify types" <+> text (show t) <+> text "and" <+> text (show t')
output (MiscError s)               = text s
