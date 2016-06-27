module Language.Kiff.Syntax where


data Program tag = Program [TypeDecl] [Def tag] deriving Show                 

data Def tag = Def tag VarName (Maybe Ty) [DefEq tag] deriving Show
data DefEq tag = DefEq tag [Pat tag] (Expr tag) deriving Show               

type DataName = String
type TvName = String
type TvId = Integer
    
data TyPrimitive  = TyInt
                  | TyBool
                  deriving (Eq, Show)

data Tv  = TvName TvName
         | TvId TvId
           deriving (Eq, Ord)

data Ty  = TyVar Tv
         | TyFun Ty Ty
         | TyApp Ty Ty
         | TyData DataName
         | TyList Ty
         | TyPrimitive TyPrimitive           
           
type ConName = String

data TypeDecl = DataDecl DataName [TvName] [DataCon]
              | TypeAlias DataName [TvName] Ty
              deriving Show
data DataCon = DataCon ConName [Ty] deriving Show
    
type VarName = String

data PrimitiveOp  = OpAdd
                  | OpSub
                  | OpMul
                  | OpDiv
                  | OpMod
                  | OpOr
                  | OpAnd
                  | OpEq
                  | OpNe
                  | OpLe
                  | OpLt
                  | OpGe
                  | OpGt
                  deriving Show                           
                           
data Expr tag  = Var tag VarName
               | Con tag ConName
               | App tag (Expr tag) (Expr tag)
               | Lam tag [Pat tag] (Expr tag)
               | Let tag [Def tag] (Expr tag)
               | PrimBinOp tag PrimitiveOp (Expr tag) (Expr tag)
               | IfThenElse tag (Expr tag) (Expr tag) (Expr tag)
               | IntLit tag Int
               | BoolLit tag Bool
               | UnaryMinus tag (Expr tag)
               | Not tag (Expr tag)
               deriving Show

data Pat tag = PVar tag VarName
             | PApp tag ConName [Pat tag]
             | Wildcard tag
             | IntPat tag Int
             | BoolPat tag Bool
             deriving Show


                                  
instance Functor Program where
    fmap f (Program decls defs) = Program decls (map (fmap f) defs)

instance Functor Def where
    fmap f (Def tag name decl eqs) = Def (f tag) name decl (map (fmap f) eqs)

instance Functor DefEq where
    fmap f (DefEq tag pats body) = DefEq (f tag) (map (fmap f) pats) (fmap f body)

instance Functor Expr where
    fmap f (Var tag var)                  = Var (f tag) var
    fmap f (Con tag con)                  = Con (f tag) con
    fmap f (App tag fun x)                = App (f tag) (fmap f fun) (fmap f x)
    fmap f (Lam tag pats body)            = Lam (f tag) (map (fmap f) pats) (fmap f body)
    fmap f (Let tag defs body)            = Let (f tag) (map (fmap f) defs) (fmap f body)
    fmap f (PrimBinOp tag op left right)  = PrimBinOp (f tag) op (fmap f left) (fmap f right)
    fmap f (IfThenElse tag cond thn els)  = IfThenElse (f tag) (fmap f cond) (fmap f thn) (fmap f els)
    fmap f (IntLit tag n)                 = IntLit (f tag) n
    fmap f (BoolLit tag b)                = BoolLit (f tag) b
    fmap f (UnaryMinus tag expr)          = UnaryMinus (f tag) (fmap f expr)
    fmap f (Not tag expr)                 = Not (f tag) (fmap f expr)

instance Functor Pat where
    fmap f (PVar tag var)       = PVar (f tag) var
    fmap f (PApp tag con pats)  = PApp (f tag) con (map (fmap f) pats)
    fmap f (Wildcard tag)       = Wildcard (f tag)
    fmap f (IntPat tag n)       = IntPat (f tag) n
    fmap f (BoolPat tag b)      = BoolPat (f tag) b



class Functor d => Tagged d where
    getTag :: d a -> a

instance Tagged Def where
    getTag (Def tag _ _ _) = tag

instance Tagged DefEq where
    getTag (DefEq tag _ _) = tag

instance Tagged Expr where
    getTag (Var tag _)             = tag
    getTag (Con tag _)             = tag
    getTag (App tag _ _)           = tag
    getTag (Lam tag _ _)           = tag
    getTag (Let tag _ _)           = tag
    getTag (PrimBinOp tag _ _ _)   = tag
    getTag (IfThenElse tag _ _ _)  = tag
    getTag (IntLit tag _)          = tag
    getTag (BoolLit tag _)         = tag
    getTag (UnaryMinus tag _)      = tag
    getTag (Not tag _)             = tag

instance Tagged Pat where
    getTag (PVar tag _)    = tag
    getTag (PApp tag _ _)  = tag
    getTag (Wildcard tag)  = tag
    getTag (IntPat tag _)  = tag
    getTag (BoolPat tag _) = tag


instance Show Tv where
    show (TvName v) = v
    show (TvId x) = "t" ++ (show x)

instance Show Ty where
    show ty = show' False ty
        where show' _p (TyVar v) = show v
              show' p (TyFun t t') = parenIf p $ unwords [show' True t, "->", show' False t']
              show' _p (TyList t) = "[" ++ (show' False t) ++ "]"
              show' _p (TyApp t t') = unwords [show' True t, show' False t']
              show' _p (TyData d) = d
              show' _p (TyPrimitive TyInt) = "Int"
              show' _p (TyPrimitive TyBool) = "Bool"

              parenIf False s = s
              parenIf True  s = "(" ++ s ++ ")"
