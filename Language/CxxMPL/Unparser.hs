module Language.CxxMPL.Unparser where

import Language.CxxMPL.Syntax
import Text.PrettyPrint

unparseMetaTy :: MetaTy -> Doc
unparseMetaTy MetaInt = text "int"
unparseMetaTy MetaBool = text "bool"
unparseMetaTy MetaTypename = text "typename"

unparseTy :: Ty -> Doc
unparseTy TyInt = text "int"
unparseTy TyBool = text "bool"
                             
langle = char '<'
rangle = char '>'         
angles d = langle <+> d <+> rangle
anglelist = angles . hsep . punctuate comma                   

struct :: String -> Doc            
struct name = text "struct" <+> varname name

varname name = text name
              
field :: String -> Ty -> Expr -> Doc
field name ty expr = hsep [text "static const",
                           unparseTy ty,
                           varname name,
                           equals,
                           unparseExpr expr,
                           semi]

bool :: Bool -> Doc              
bool True   = text "true"
bool False  = text "false"

template :: Bool -> [Doc] -> Doc
template  False  []    = empty
template _      docs  = text "template" <+> anglelist docs

unparseMetaDecl :: MetaDecl -> Doc
unparseMetaDecl (MetaDecl name mtys) = template False (map unparseMetaTy mtys) <+> struct name <> semi

unparseMetaVarDecl :: MetaVarDecl -> Doc
unparseMetaVarDecl (MetaVarDecl name mty) = unparseMetaTy mty <+> varname name

unparseMetaBody :: (Ty, Expr) -> Doc
unparseMetaBody (ty, expr) = field "v" ty expr

unparseField (Field name (ty, expr)) = field name ty expr                             
                             
unparseMetaDef :: MetaDef -> Doc
unparseMetaDef mdef = vcat [template True formals,
                            struct name <> anglelist (map unparseMetaExpr spec),
                            lbrace,
                            nest 2 $ vcat (map unparseField fields) $+$ unparseMetaBody body,
                            rbrace] <> semi
    where formals = map unparseMetaVarDecl $ mdefFormals mdef
          name = mdefName mdef
          spec = mdefSpec mdef
          body = mdefBody mdef
          fields = mdefFields mdef

unparseMetaExpr (MetaVar var)      = text var
unparseMetaExpr (MetaBoolLit b)    = bool b
unparseMetaExpr (MetaIntLit i)     = int i
unparseMetaExpr (MetaCall var es)  = text var <> anglelist (map unparseMetaExpr es)

unparseExpr (FormalRef v) = varname v
unparseExpr (VarRef v) = varname v
unparseExpr (Cons cons args) = varname cons <+> anglelist (map unparseExpr args)
unparseExpr (Call fun args) = varname fun <+> anglelist (map unparseExpr args) <> colon <> colon <> text "v"
unparseExpr (IntLit i) = int i
unparseExpr (PrimBinOp op left right) = unparseExpr left <+> unparseOp op <+> unparseExpr right

unparseOp OpAdd  = text "+"
unparseOp OpEq   = text "=="
unparseOp OpMod  = text "%"
                                     
testDecl = MetaDecl "search_i" [MetaInt, MetaBool, MetaBool, MetaTypename]
testDef = MetaDef { mdefName = "search_i",
                    mdefFormals = [MetaVarDecl "length" MetaInt,
                                   MetaVarDecl "good" MetaBool,
                                   MetaVarDecl "final" MetaBool,
                                   MetaVarDecl "digit" MetaInt,
                                   MetaVarDecl "rest" MetaTypename],
                    mdefSpec = [MetaVar "length",
                                MetaVar "good",
                                MetaVar "final",
                                MetaCall "Cons" [MetaVar "digit", MetaVar "rest"]],
                    mdefFields = [],
                    mdefBody = (TyInt, Call "search" [FormalRef "length", Cons "Cons" [PrimBinOp OpAdd (FormalRef "digit") (IntLit 1), FormalRef "rest"]])
                  }
                               
                               
testDef' = MetaDef { mdefName = "divisor_test",
                     mdefFormals = [MetaVarDecl "first" MetaInt,
                                    MetaVarDecl "rest" MetaTypename],
                    mdefSpec = [MetaCall "Cons" [MetaVar "first", MetaVar "rest"]],
                    mdefFields = [Field "num" (TyInt, Call "value" [Cons "Cons" [FormalRef "first", FormalRef "rest"]]),
                                  Field "div" (TyInt, Call "length" [Cons "Cons" [FormalRef "first", FormalRef "rest"]]),
                                  Field "mod" (TyInt, PrimBinOp OpMod (VarRef "num") (VarRef "div"))],
                    mdefBody = (TyBool, PrimBinOp OpEq (VarRef "mod") (IntLit 0))
                  }
                               
                               
