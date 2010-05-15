module Language.CxxMPL.Unparser (unparse) where

import Language.CxxMPL.Syntax

import Text.PrettyPrint
import Data.List
import Data.Maybe
    
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
field name TyClass expr = hsep [text "typedef",
                                unparse expr,
                                varname name] <> semi
field name ty      expr = hsep [text "static const",
                                unparse ty,
                                varname name,
                                equals,
                                unparse expr] <> semi

template :: Bool -> [Doc] -> Doc
template  False  []    = empty
template  _      docs  = text "template" <> anglelist docs

bool :: Bool -> Doc              
bool True   = text "true"
bool False  = text "false"

instance (Unparse MetaTy) where
    unparse MetaInt           = text "int"
    unparse MetaBool          = text "bool"
    unparse (MetaClass mtys)  = template False (map unparse mtys) <+> text "class"

instance (Unparse Ty) where
    unparse TyInt   = text "int"
    unparse TyBool  = text "bool" 
                      
instance (Unparse Field) where                             
    unparse (Field name (ty, expr)) = field name ty expr                             

instance (Unparse Expr) where
    unparse (Typename expr)            = text "typename" <+> unparse expr
    unparse (VarRef v)                 = varname v              
    unparse (Cons cons [])             = varname cons
    unparse (Cons cons args)           = varname cons <> anglelist (map unparse args)
    unparse (Call fun args)            = varname fun <> anglelist (map unparse args) <> text "::v"
    unparse (IntLit i)                 = int i
    unparse (BoolLit True)             = text "true"
    unparse (BoolLit False)            = text "false"
    unparse (Cond cond thn els)        = parens (unparse cond) <+> text "?" <+> parens (unparse thn) <+> colon <+> parens (unparse els)
    unparse (PrimBinOp op left right)  = parens (unparse left) <+> unparse op <+> parens (unparse right)
    unparse (Not expr)                 = text "!" <> parens (unparse expr)
    unparse (UnaryMinus expr)          = parens $ text "-" <> parens (unparse expr)
    unparse (Box TyInt expr)           = unparse $ Cons "Int" [expr]
    unparse (Box TyBool expr)          = unparse $ Cons "Bool" [expr]
    unparse (Unbox expr)               = unparse expr <> text "::v"
                                         
instance (Unparse PrimitiveOp) where
    unparse OpAdd  = text "+"
    unparse OpSub  = text "-"
    unparse OpMul  = text "*"
    unparse OpDiv  = text "/"
    unparse OpMod  = text "%"
    unparse OpEq   = text "=="
    unparse OpNe   = text "!="
    unparse OpLe   = text "<="
    unparse OpLt   = text "<"
    unparse OpGe   = text ">="
    unparse OpGt   = text ">"
    unparse OpOr   = text "||"
    unparse OpAnd  = text "&&"

instance (Unparse MetaVarDecl) where
    unparse (MetaVarDecl name mty) = unparse mty <+> varname name
                     
instance (Unparse Program) where
    unparse (Program defs) = unparseDecls $+$ text " " $+$ unparseDefs
        where unparseDecls = foldl ($+$) empty (map unparseDecl defs)
              unparseDefs = foldl ($+$) empty $ intersperse (text "") $ concatMap unparseDef defs
                     
              unparseDecl (Def name mtys specs) = template False (map unparse mtys) <+> struct name <> semi
              unparseDef (Def name mtys specs) = map (unparseSpec name) specs
              unparseSpec name (Specialization vars spec fields (ty, body)) = vcat [template (isJust spec) $ map unparse vars,
                                                                                    struct name <> maybe empty (anglelist . map unparse) spec,
                                                                                    lbrace,
                                                                                    nest 2 $ vcat (map unparse fields) $+$ field "v" ty body,
                                                                                    rbrace] <> semi
        
testDef = Def "divisor_test" [MetaClass []] [testSpec]
testSpec = Specialization [MetaVarDecl "first" (MetaClass []), MetaVarDecl "rest" (MetaClass [])]
                          (Just [Call "cons" [VarRef "first", VarRef "rest"]])
                          [Field "num" (TyInt, Call "value" [Cons "cons" [VarRef "first", VarRef "rest"]]),
                           Field "div" (TyInt, Call "length" [Cons "cons" [VarRef "first", VarRef "rest"]]),
                           Field "mod" (TyInt, PrimBinOp OpMod (VarRef "num") (VarRef "div"))]
                          (TyInt, PrimBinOp OpEq (VarRef "mod") (IntLit 0))

nilDef = Def "nil" [] []

intDef = Def "Int" [MetaInt] [Specialization [MetaVarDecl "x" MetaInt] Nothing [] (TyInt, VarRef "x")]
boolDef = Def "Bool" [MetaBool] [Specialization [MetaVarDecl "b" MetaBool] Nothing [] (TyBool, VarRef "b")]
                           
         
         
testProg = Program [intDef, boolDef]
