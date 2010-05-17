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
    fmap f (Def xi name decl eqs) = Def (f xi) name decl (map (fmap f) eqs)               

instance Functor DefEq where
    fmap f (DefEq xi pats body) = DefEq (f xi) (map (fmap f) pats) (fmap f body)
                
instance Functor Expr where
    fmap f (Var xi var)                  = Var (f xi) var
    fmap f (Con xi con)                  = Con (f xi) con
    fmap f (App xi fun x)                = App (f xi) (fmap f fun) (fmap f x)
    fmap f (Lam xi pats body)            = Lam (f xi) (map (fmap f) pats) (fmap f body)
    fmap f (Let xi defs body)            = Let (f xi) (map (fmap f) defs) (fmap f body)
    fmap f (PrimBinOp xi op left right)  = PrimBinOp (f xi) op (fmap f left) (fmap f right)
    fmap f (IfThenElse xi cond thn els)  = IfThenElse (f xi) (fmap f cond) (fmap f thn) (fmap f els)
    fmap f (IntLit xi n)                 = IntLit (f xi) n
    fmap f (BoolLit xi b)                = BoolLit (f xi) b
    fmap f (UnaryMinus xi expr)          = UnaryMinus (f xi) (fmap f expr)
    fmap f (Not xi expr)                 = Not (f xi) (fmap f expr)    

instance Functor Pat where
    fmap f (PVar xi var)       = PVar (f xi) var
    fmap f (PApp xi con pats)  = PApp (f xi) con (map (fmap f) pats)
    fmap f (Wildcard xi)       = Wildcard (f xi)
    fmap f (IntPat xi n)       = IntPat (f xi) n
    fmap f (BoolPat xi b)      = BoolPat (f xi) b
                        


class Functor d => Tagged d where
    getTag :: d a -> a

instance Tagged Def where
    getTag (Def xi _ _ _) = xi

instance Tagged DefEq where
    getTag (DefEq xi _ _) = xi

instance Tagged Expr where
    getTag (Var xi _)             = xi
    getTag (Con xi _)             = xi
    getTag (App xi _ _)           = xi
    getTag (Lam xi _ _)           = xi
    getTag (Let xi _ _)           = xi
    getTag (PrimBinOp xi _ _ _)   = xi
    getTag (IfThenElse xi _ _ _)  = xi
    getTag (IntLit xi _)          = xi
    getTag (BoolLit xi _)         = xi
    getTag (UnaryMinus xi _)      = xi
    getTag (Not xi expr)          = xi

instance Tagged Pat where
    getTag (PVar xi _)    = xi
    getTag (PApp xi _ _)  = xi
    getTag (Wildcard xi)  = xi
    getTag (IntPat xi _)  = xi
    getTag (BoolPat xi _) = xi
                                   
    
instance Show Tv where
    show (TvName v) = v
    show (TvId x) = "t" ++ (show x)
                    
instance Show Ty where
    show ty = show' False ty
        where show' p (TyVar v) = show v
              show' p (TyFun t t') = parenIf p $ unwords [show' True t, "->", show' False t']
              show' p (TyList t) = "[" ++ (show' False t) ++ "]"
              show' p (TyApp t t') = unwords [show' True t, show' False t']
              show' p (TyData d) = d
              show' p (TyPrimitive TyInt) = "Int"
              show' p (TyPrimitive TyBool) = "Bool"

              parenIf False s = s
              parenIf True  s = "(" ++ s ++ ")"
                  

                                  
