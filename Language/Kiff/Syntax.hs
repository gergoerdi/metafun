module Language.Kiff.Syntax where

type DataName = String
type TvName = String
type TvId = Int
    
data TyPrimitive  = TyInt
                  | TyBool
                  deriving (Eq, Show)

data Tv  = TvName TvName
         | TvId TvId
           deriving (Eq, Ord)

instance Show Tv where
    show (TvName v) = v
    show (TvId x) = "t" ++ (show x)
                    
data Ty  = TyVar Tv
         | TyFun Ty Ty
         | TyApp Ty Ty
         | TyData DataName
         | TyList Ty
         | TyPrimitive TyPrimitive           
           
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
                  

type VarName = String
type ConName = String

data TypeDecl = DataDecl DataName [TvName] [DataCon]
              | TypeAlias DataName [TvName] Ty
              deriving Show
data DataCon = DataCon ConName [Ty] deriving Show
    
data PrimitiveOp  = OpAdd
                  | OpSub
                  | OpMul
                  | OpDiv
                  | OpMod
                  | OpOr
                  | OpAnd
                  | OpEq
                  | OpLe
                  | OpLt
                  | OpGe
                  | OpGt
                  deriving Show                           
                           
data Expr  tag = Var tag VarName
               | Con tag ConName
               | App tag (Expr tag) (Expr tag)
               | Lam tag [Pat tag] (Expr tag)
               | Let tag [Def tag] (Expr tag)
               | PrimBinOp tag PrimitiveOp (Expr tag) (Expr tag)
               | IfThenElse tag (Expr tag) (Expr tag) (Expr tag)
               | IntLit tag Int
               | BoolLit tag Bool
               | UnaryMinus tag (Expr tag)
               deriving Show

type TExpr = Expr Ty                        
                      
data Pat tag = PVar tag VarName
             | PApp tag ConName [Pat tag]
             | Wildcard tag
             | IntPat tag Int
             | BoolPat tag Bool
             deriving Show

type TPat = Pat Ty
                     
data Def tag = Def tag VarName (Maybe Ty) [DefEq tag] deriving Show
data DefEq tag = DefEq tag [Pat tag] (Expr tag) deriving Show

type TDef = Def Ty
type TDefEq = DefEq Ty
         
data Program tag = Program [TypeDecl] [Def tag] deriving Show
type TProgram = Program Ty
