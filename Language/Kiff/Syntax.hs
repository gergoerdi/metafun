module Language.Kiff.Syntax where

type DataName = String
type TvName = String
    
data TyPrimitive  = TyInt
                  | TyBool
                    deriving Show

data Ty  = TyVar TvName
         | TyFun Ty Ty
         | TyApp Ty Ty
         | TyData DataName
         | TyPrimitive TyPrimitive
           deriving Show


type VarName = String
type ConName = String
    
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
           | PrimBinOp PrimitiveOp Expr Expr
           | IfThenElse Expr Expr Expr
           | IntLit Int
           | BoolLit Bool
             deriving Show

data Pat  = PVar VarName
          | PApp ConName [Pat]
          | IntPat Int
          | BoolPat Bool
            deriving Show
                     
data DefEq = DefEq [Pat] Expr deriving Show
data Def = Def VarName (Maybe Ty) [DefEq] deriving Show
