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
                           
data Expr  = Var VarName
           | Con ConName
           | App Expr Expr
           | Lam [Pat] Expr
           | Let [Def] Expr
           | PrimBinOp PrimitiveOp Expr Expr
           | IfThenElse Expr Expr Expr
           | IntLit Int
           | BoolLit Bool
           | UnaryMinus Expr
           deriving Show

-- Typed expression (result of type inference)
data TExpr = TVar Ty VarName
           | TCon Ty ConName
           | TApp Ty TExpr TExpr
           | TLam Ty [TPat] TExpr
           | TLet Ty [TDef] TExpr
           | TPrimBinOp Ty PrimitiveOp TExpr TExpr
           | TIfThenElse Ty TExpr TExpr TExpr
           | TIntLit Int
           | TBoolLit Bool
           | TUnaryMinus TExpr
           deriving Show
                      
data Pat  = PVar VarName
          | PApp ConName [Pat]
          | Wildcard
          | IntPat Int
          | BoolPat Bool
          deriving Show

-- Typed pattern
data TPat = TPVar Ty VarName
          | TPApp Ty ConName [TPat]
          | TWildcard Ty
          | TIntPat Int
          | TBoolPat Bool
          deriving Show
                     
data DefEq = DefEq [Pat] Expr deriving Show
data Def = Def VarName (Maybe Ty) [DefEq] deriving Show

data TDefEq = TDefEq Ty [TPat] TExpr deriving Show
data TDef = TDef Ty VarName (Maybe Ty) [TDefEq] deriving Show         
         
data Program = Program [TypeDecl] [Def] deriving Show
data TProgram = TProgram [TypeDecl] [TDef] deriving Show
