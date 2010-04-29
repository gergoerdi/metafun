module Language.CxxMPL.Unparser (unparse) where

import Language.CxxMPL.Syntax
import Text.PrettyPrint
import Data.List
    
class (Unparse a) where
    unparse :: a -> Doc

langle = char '<'
rangle = char '>'         
angles d = langle <+> d <+> rangle
anglelist = angles . hsep . punctuate comma                   

struct :: String -> Doc            
struct name = text "struct" <+> varname name

varname name = text name
              
field :: String -> Ty -> Expr -> Doc
field name ty expr = hsep [text "static const",
                           unparse ty,
                           varname name,
                           equals,
                           unparse expr,
                           semi]

template :: Bool -> [Doc] -> Doc
template  False  []    = empty
template  _      docs  = text "template" <> anglelist docs

bool :: Bool -> Doc              
bool True   = text "true"
bool False  = text "false"

instance (Unparse MetaTy) where
    unparse MetaInt           = text "int"
    unparse MetaBool          = text "bool"
    unparse (MetaClass mtys)  = (template False $ (map unparse mtys)) <+> text "class"

instance (Unparse Ty) where
    unparse TyInt   = text "int"
    unparse TyBool  = text "bool" 

instance (Unparse MetaDecl) where
    unparse (MetaDecl name mtys) = template False (map unparse mtys) <+> struct name <> semi

instance (Unparse MetaVarDecl) where                                
    unparse (MetaVarDecl name mty) = unparse mty <+> varname name

instance (Unparse Field) where                             
    unparse (Field name (ty, expr)) = field name ty expr                             

instance (Unparse MetaDef) where                                      
    unparse mdef = vcat [template True formals,
                         struct name <> anglelist (map unparse spec),
                         lbrace,
                         nest 2 $ vcat (map unparse fields) $+$ field "v" ty body,
                         rbrace] <> semi
        where formals = map unparse $ mdefFormals mdef
              name = mdefName mdef
              spec = mdefSpec mdef
              (ty, body) = mdefBody mdef
              fields = mdefFields mdef              

instance (Unparse MetaExpr) where                     
    unparse (MetaVar var)      = text var
    unparse (MetaBoolLit b)    = bool b
    unparse (MetaIntLit i)     = int i
    unparse (MetaCall var es)  = text var <> anglelist (map unparse es)

instance (Unparse Expr) where                                 
    unparse (FormalRef v)              = varname v
    unparse (VarRef v)                 = varname v
    unparse (Cons cons args)           = varname cons <> anglelist (map unparse args)
    unparse (Call fun args)            = varname fun <> anglelist (map unparse args) <> colon <> colon <> text "v"
    unparse (IntLit i)                 = int i
    unparse (BoolLit True)             = text "true"
    unparse (BoolLit False)            = text "false"
    unparse (PrimBinOp op left right)  = parens (unparse left) <+> unparse op <+> parens (unparse right)
    unparse (UnaryMinus expr)          = parens (text "-" <> parens (unparse expr))
                                         
instance (Unparse PrimitiveOp) where
    unparse OpAdd  = text "+"
    unparse OpEq   = text "=="
    unparse OpMod  = text "%"
    unparse OpMul  = text "*"
    unparse OpOr   = text "||"
    unparse OpAnd  = text "&&"

instance (Unparse Program) where
    unparse (Program metadecls metadefs) = foldl ($+$) empty docs
        where docs = map unparse metadecls ++ [text ""] ++ (intersperse (text "") $ map unparse metadefs)
        
testDecl = MetaDecl "search_i" [MetaInt, MetaBool, MetaBool, (MetaClass [])]
testDef = MetaDef { mdefName = "search_i",
                    mdefFormals = [MetaVarDecl "length" MetaInt,
                                   MetaVarDecl "good" MetaBool,
                                   MetaVarDecl "final" MetaBool,
                                   MetaVarDecl "digit" MetaInt,
                                   MetaVarDecl "rest" (MetaClass [])],
                    mdefSpec = [MetaVar "length",
                                MetaVar "good",
                                MetaVar "final",
                                MetaCall "Cons" [MetaVar "digit", MetaVar "rest"]],
                    mdefFields = [],
                    mdefBody = (TyInt, Call "search" [FormalRef "length", Cons "Cons" [PrimBinOp OpAdd (FormalRef "digit") (IntLit 1), FormalRef "rest"]])
                  }
                               

testDecl' = MetaDecl "divisor_test" [MetaClass []]
          
testDef' = MetaDef { mdefName = "divisor_test",
                     mdefFormals = [MetaVarDecl "first" MetaInt,
                                    MetaVarDecl "rest" (MetaClass [])],
                     mdefSpec = [MetaCall "Cons" [MetaVar "first", MetaVar "rest"]],
                     mdefFields = [Field "num" (TyInt, Call "value" [Cons "Cons" [FormalRef "first", FormalRef "rest"]]),
                                   Field "div" (TyInt, Call "length" [Cons "Cons" [FormalRef "first", FormalRef "rest"]]),
                                   Field "mod" (TyInt, PrimBinOp OpMod (VarRef "num") (VarRef "div"))],
                     mdefBody = (TyBool, PrimBinOp OpEq (VarRef "mod") (IntLit 0))
                  }
                               
                               
testProg = Program [testDecl'] [testDef']
