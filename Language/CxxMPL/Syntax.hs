module Language.CxxMPL.Syntax where

type VarName = String
type MetaVarName = VarName    
    
data MetaVarDecl = MetaVarDecl MetaVarName MetaTy deriving Show
                 
data MetaTy  =  MetaInt
             |  MetaBool
             |  MetaClass [MetaTy]
             deriving Show

data Ty = TyInt
        | TyBool
        | TyClass
        deriving (Show, Eq)
                      
data MetaDecl = MetaDecl MetaVarName [MetaTy] deriving Show

data TypedExpr = TypedExpr Ty Expr              
              
data MetaDef = MetaDef { mdefName :: MetaVarName,
                         mdefFormals :: [MetaVarDecl],
                         mdefSpec :: MetaSpecialization,
                         mdefFields :: [Field],
                         mdefBody :: (Ty, Expr) } deriving Show

data Field = Field VarName (Ty, Expr) deriving Show

type MetaSpecialization = Maybe [MetaExpr]

data MetaExpr  = MetaVar MetaVarName
               | MetaBoolLit Bool
               | MetaIntLit Int
               | MetaCall MetaVarName [MetaExpr]
               | MetaBox Ty MetaExpr
               deriving Show

data Expr  = Typename Expr
           | PrimBinOp PrimitiveOp Expr Expr
           | Not Expr
           | Call MetaVarName [Expr]
           | Cons MetaVarName [Expr]
           | FormalRef MetaVarName
           | VarRef VarName
           | BoolLit Bool
           | IntLit Int
           | UnaryMinus Expr
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

data Program = Program [MetaDecl] [MetaDef]
