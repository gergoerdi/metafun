module Language.CxxMPL.Syntax where

type VarName = String
type MetaVarName = VarName    
    
data MetaVarDecl = MetaVarDecl MetaVarName MetaTy deriving Show
                 
data MetaTy  =  MetaInt
             |  MetaBool
             |  MetaTypename
             deriving Show

data Ty = TyInt
        | TyBool
        deriving Show
                      
data MetaDecl = MetaDecl MetaVarName [MetaTy] deriving Show

data TypedExpr = TypedExpr Ty Expr              
              
data MetaDef = MetaDef { mdefName :: MetaVarName,
                         mdefFormals :: [MetaVarDecl],
                         mdefSpec :: [MetaSpecialization],
                         mdefFields :: [Field],
                         mdefBody :: (Ty, Expr) } deriving Show

data Field = Field VarName (Ty, Expr) deriving Show

type MetaSpecialization = MetaExpr

data MetaExpr  = MetaVar MetaVarName
               | MetaBoolLit Bool
               | MetaIntLit Int
               | MetaCall MetaVarName [MetaExpr] deriving Show

data Expr  = PrimBinOp PrimitiveOp Expr Expr
           | Call MetaVarName [Expr]
           | Cons MetaVarName [Expr]
           | FormalRef MetaVarName
           | VarRef VarName
           | BoolLit Bool
           | IntLit Int
           | UnaryMinus Expr
           deriving Show

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

data Program = Program [MetaDecl] [MetaDef]
