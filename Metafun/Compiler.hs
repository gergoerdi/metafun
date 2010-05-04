module Metafun.Compiler where

import qualified Language.Kiff.Syntax as Kiff
import Language.Kiff.Typing
import qualified Language.Kiff.Typing.Substitution as Subst
    
import qualified Language.CxxMPL.Syntax as MPL

import Metafun.Compiler.State
import Control.Monad
import Data.List
    
import Debug.Trace

type TProgram = Kiff.Program Kiff.Ty
type TDef = Kiff.Def Kiff.Ty
type TDefEq = Kiff.DefEq Kiff.Ty
type TExpr = Kiff.Expr Kiff.Ty
type TPat = Kiff.Pat Kiff.Ty
    
compileDef :: TDef -> Compile [MPL.MetaDef]
compileDef (Kiff.Def tau name _ eqs) = mapM (compileDefEq name) eqs

compileDefEq :: Kiff.VarName -> TDefEq -> Compile MPL.MetaDef
compileDefEq name (Kiff.DefEq tau pats expr) = do
  (formalss, specs) <- liftM unzip $ mapM varsAndSpec pats
  let specs' = if (all isVar specs) then Nothing else Just specs
          where isVar (MPL.MetaVar _) = True
                isVar _               = False
      formals = concat formalss
      tyRet = last $ uncurryTy tau
  body <- compileExpr expr
  let body' = case formals of
                [] -> body
                _  -> addTypename body
  return $ MPL.MetaDef { MPL.mdefName = name,
                         MPL.mdefFormals = formals,
                         MPL.mdefSpec = specs',
                         MPL.mdefFields = [],
                         MPL.mdefBody = (MPL.TyClass, body') }

addTypename :: MPL.Expr -> MPL.Expr
addTypename (MPL.Unbox (MPL.Call fun args)) = MPL.Unbox (MPL.Call fun (map addTypename args))
addTypename (MPL.Unbox e)                   = MPL.Unbox $ addTypename e
addTypename (MPL.Box tau e)                   = MPL.Box tau $ addTypename e
addTypename (MPL.PrimBinOp op l r)          = MPL.PrimBinOp op (addTypename l) (addTypename r)
addTypename (MPL.Not e)                     = MPL.Not $ addTypename e
addTypename (MPL.Call fun args)             = MPL.Typename $ MPL.Call fun $ map addTypename args
addTypename (MPL.Cons con args)             = MPL.Cons con $ map addTypename args
addTypename e@(MPL.FormalRef _)             = e
addTypename e@(MPL.VarRef _)                = e
addTypename e@(MPL.BoolLit _)               = e
addTypename e@(MPL.IntLit _)                = e
addTypename (MPL.UnaryMinus e)              = MPL.UnaryMinus $ addTypename e                                                                                
convertTy (Kiff.TyPrimitive Kiff.TyInt)  = MPL.TyInt
convertTy (Kiff.TyPrimitive Kiff.TyBool) = MPL.TyBool

compileTypeDecl :: Kiff.TypeDecl -> Compile [MPL.MetaDecl]
compileTypeDecl (Kiff.DataDecl name tvnames cons) = mapM compileDataCon cons
                    
unbox :: MPL.Expr -> MPL.Expr
unbox (MPL.Box _ expr) = expr
unbox expr             = MPL.Unbox expr
                           

compileDataCon :: Kiff.DataCon -> Compile MPL.MetaDecl
compileDataCon (Kiff.DataCon name tys) = do
  mtys <- mapM compileMetaTy tys
  return $ MPL.MetaDecl name mtys

                                            
compile :: TProgram -> Compile MPL.Program
compile (Kiff.Program typeDecls defs) = do
  mtyDecls <- concatMapM compileTypeDecl (listDecl:typeDecls)
  mvarDecls <- mapM mvarDecl defs
  metaDefs <- concatMapM compileDef defs
  let metaDecls = mtyDecls ++ mvarDecls
  return $ MPL.Program (platformDecls ++ metaDecls) (platformDefs ++ metaDefs)
      where listDecl = Kiff.DataDecl "list" ["a"] [nil,cons]
                where nil = Kiff.DataCon "nil" []
                      cons = Kiff.DataCon "cons" [Kiff.TyVar (Kiff.TvName "a"), Kiff.TyList $ Kiff.TyVar (Kiff.TvName "a")]

            mvarDecl (Kiff.Def tau name _ _) = liftM (MPL.MetaDecl name) $ mapM compileMetaTy tys
              where tys = init $ uncurryTy tau
            platformDecls = [MPL.MetaDecl "Int" [MPL.MetaInt],
                             MPL.MetaDecl "Bool" [MPL.MetaBool]]
            platformDefs = [MPL.MetaDef { MPL.mdefName = "Int",
                                          MPL.mdefFormals = [MPL.MetaVarDecl "x" MPL.MetaInt],
                                          MPL.mdefSpec = Nothing,
                                          MPL.mdefFields = [],
                                          MPL.mdefBody = (MPL.TyInt, MPL.FormalRef "x") },
                            MPL.MetaDef { MPL.mdefName = "Bool",
                                          MPL.mdefFormals = [MPL.MetaVarDecl "b" MPL.MetaBool],
                                          MPL.mdefSpec = Nothing,
                                          MPL.mdefFields = [],
                                          MPL.mdefBody = (MPL.TyBool, MPL.FormalRef "b") }]
                           

uncurryApp :: TExpr -> [TExpr]
uncurryApp (Kiff.App _ f x) = (uncurryApp f) ++ [x]
uncurryApp expr             = [expr]                              

compileExpr :: TExpr -> Compile MPL.Expr
compileExpr (Kiff.Var tau var) = return $ MPL.FormalRef var
compileExpr (Kiff.Con tau con) = return $ MPL.Cons con []
compileExpr e@(Kiff.App tau f x) = do
  let (fun:args) = uncurryApp e
      mpl = case fun of
              Kiff.Var _ var -> MPL.Call var
              Kiff.Con tau con -> MPL.Cons con
  args' <- mapM compileExpr args
  return $ mpl args'
compileExpr (Kiff.Lam tau pats body) = error "Lambdas not supported"
compileExpr (Kiff.Let tau defs body) = undefined
compileExpr (Kiff.PrimBinOp tau op left right) = do
  left' <- liftM unbox $ compileExpr left
  right' <- liftM unbox $ compileExpr right
  let op' = compileOp op
  return $ MPL.Box (convertTy tau) $ MPL.PrimBinOp op' left' right'
compileExpr (Kiff.IfThenElse tau cond thn els) = undefined
compileExpr (Kiff.IntLit _ n) = return $ MPL.Box MPL.TyInt $ MPL.IntLit n
compileExpr (Kiff.BoolLit _ b) = return $ MPL.Box MPL.TyBool $ MPL.BoolLit b
compileExpr (Kiff.UnaryMinus _ e) = do
  e' <- liftM unbox $ compileExpr e
  return $ MPL.Box MPL.TyInt $ MPL.UnaryMinus e'
compileExpr (Kiff.Not _ e) = do
  e' <- liftM unbox $ compileExpr e      
  return $ MPL.Box MPL.TyBool $ MPL.Not e'

compileMetaTy :: Kiff.Ty -> Compile MPL.MetaTy
compileMetaTy tau@(Kiff.TyFun x y)              = do
  mtys <- mapM compileMetaTy $ init $ uncurryTy tau
  return $ MPL.MetaClass mtys
compileMetaTy _                               = return $ MPL.MetaClass []

compileOp :: Kiff.PrimitiveOp -> MPL.PrimitiveOp -- TODO: use the same def in both languages
compileOp Kiff.OpAdd = MPL.OpAdd
compileOp Kiff.OpSub = MPL.OpSub
compileOp Kiff.OpMul = MPL.OpMul
compileOp Kiff.OpDiv = MPL.OpDiv
compileOp Kiff.OpMod = MPL.OpMod
compileOp Kiff.OpEq  = MPL.OpEq
compileOp Kiff.OpGt  = MPL.OpGt
compileOp Kiff.OpGe  = MPL.OpGe
compileOp Kiff.OpLe  = MPL.OpLe
compileOp Kiff.OpLt  = MPL.OpLt
compileOp Kiff.OpAnd = MPL.OpAnd
compileOp Kiff.OpOr  = MPL.OpOr
                       
varsAndSpec :: TPat -> Compile ([MPL.MetaVarDecl], MPL.MetaSpecialization)
varsAndSpec (Kiff.PVar tau var)       = do
  mty <- compileMetaTy tau
  return ([MPL.MetaVarDecl var mty], MPL.MetaVar var)
varsAndSpec (Kiff.PApp tau con pats)  = do
  (mdecls, mspecs) <- liftM unzip $ mapM varsAndSpec pats
  return (concat mdecls, MPL.MetaCall con mspecs)
varsAndSpec (Kiff.Wildcard tau)       = do
  mv <- newMetaVarName
  varsAndSpec (Kiff.PVar tau mv)
varsAndSpec (Kiff.IntPat _ n)       = return ([], MPL.MetaBox MPL.TyInt $ MPL.MetaIntLit n)
varsAndSpec (Kiff.BoolPat _ b)      = return ([], MPL.MetaBox MPL.TyBool $ MPL.MetaBoolLit b)

uncurryTy (Kiff.TyFun t t')  = (t:uncurryTy t')
uncurryTy t                  = [t]

uncurryTyApp (Kiff.TyApp t t') = (uncurryTyApp t) ++ [t']
uncurryTyApp t                 = [t]
