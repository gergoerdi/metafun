module Language.CxxMPL.Syntax where

type Name = String
    
data Def = Def Name [MetaTy] [Specialization] deriving Show
data Specialization = Specialization [MetaVarDecl] (Maybe [Expr]) [Field] (Ty, Expr) deriving Show

data Field = Field Name (Ty, Expr) deriving Show                      

data MetaVarDecl = MetaVarDecl Name MetaTy deriving Show
                    
data Ty = TyInt
        | TyBool
        | TyClass
        deriving (Show, Eq)

data MetaTy = MetaInt
            | MetaBool
            | MetaClass [MetaTy]
            deriving Show
                 
data Expr  = Typename Expr
           | Cons Name [Expr]
           | Call Name [Expr]
           | VarRef Name
           | BoolLit Bool
           | IntLit Int
           | Cond Expr Expr Expr
           | UnaryMinus Expr
           | PrimBinOp PrimitiveOp Expr Expr
           | Not Expr
           | Box Ty Expr
           | Unbox Expr
           deriving Show

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

data Program = Program [Def] deriving Show
