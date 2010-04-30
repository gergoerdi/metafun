module Metafun.Compiler where

import qualified Language.Kiff.Syntax as Kiff
import Language.Kiff.Typing

import qualified Language.CxxMPL.Syntax as MPL

import Control.Monad.State    

compileDef :: Kiff.TDef -> [MPL.MetaDef]
compileDef (Kiff.Def tau name _ eqs) = map (compileDefEq name) eqs

compileDefEq name (Kiff.DefEq tau pats expr) =
    MPL.MetaDef { MPL.mdefName = name,
                  MPL.mdefFormals = formals,
                  MPL.mdefSpec = specs',
                  MPL.mdefFields = [],
                  MPL.mdefBody = (compileTy tyRet, compileExpr expr) }
        where tyRet = last $ uncurryTy tau
              (formalss, specs) = unzip $ map varsAndSpec pats
              specs' = if (all isVar specs) then Nothing else Just specs
                  where isVar (MPL.MetaVar _) = True
                        isVar _               = False
              formals = concat formalss
    
compileTy (Kiff.TyPrimitive Kiff.TyInt)   = MPL.TyInt
compileTy (Kiff.TyPrimitive Kiff.TyBool)  = MPL.TyBool
compileTy _ = MPL.TyClass

convertTy (Kiff.TyPrimitive Kiff.TyInt) = MPL.TyInt
convertTy (Kiff.TyPrimitive Kiff.TyBool) = MPL.TyBool
                                            
compileTypeDecl :: Kiff.TypeDecl -> [MPL.MetaDecl]
compileTypeDecl (Kiff.DataDecl name tvs cons) = map compileDataCon cons

compileDataCon :: Kiff.DataCon -> MPL.MetaDecl
compileDataCon (Kiff.DataCon name tys) = MPL.MetaDecl name (map compileMetaTy tys)

                                            
compile :: Kiff.TProgram -> MPL.Program
compile (Kiff.Program typedecls tdefs) = MPL.Program metadecls metadefs
    where metadecls = mtydecls ++ mvardecls
          metadefs = concatMap compileDef tdefs
                     
          mtydecls = concatMap compileTypeDecl (listdecl:typedecls)
              where listdecl = Kiff.DataDecl "list" ["a"] [nil,cons]
                    nil = Kiff.DataCon "nil" []
                    cons = Kiff.DataCon "cons" [Kiff.TyVar (Kiff.TvName "a"), Kiff.TyList $ Kiff.TyVar (Kiff.TvName "a")]

          mvardecls = map mvardecl tdefs
          mvardecl (Kiff.Def tau name _ _) = MPL.MetaDecl name (map compileMetaTy tys)
              where tys = init $ uncurryTy tau

uncurryApp :: Kiff.TExpr -> [Kiff.TExpr]
uncurryApp (Kiff.App _ f x)  = (uncurryApp f) ++ [x]
uncurryApp expr               = [expr]
                              
compileExpr :: Kiff.TExpr -> MPL.Expr
compileExpr (Kiff.Var tau var) = MPL.FormalRef var -- error $ unwords ["TVar", var]
compileExpr (Kiff.Con tau con) = MPL.Cons con [] -- error $ unwords ["TCon", con]
compileExpr e@(Kiff.App tau f x) = mpl $ map compileExpr args
    where (fun:args) = uncurryApp e
          mpl = case fun of
                  Kiff.Var _ var -> case tau of
                                      Kiff.TyPrimitive _  -> MPL.Call var
                                      _                   -> MPL.Typename . MPL.Call var
                  Kiff.Con _ con -> MPL.Cons con
compileExpr (Kiff.Lam tau pats body) = error "Lambdas not supported"
compileExpr (Kiff.Let tau defs body) = undefined
compileExpr (Kiff.PrimBinOp tau op left right) = box (convertTy tau) $ MPL.PrimBinOp op' left' right'
    where tLeft = getTy left
          tRight = getTy right
          tRes = tau
          op' = compileOp op
          left' = unbox (convertTy tLeft) $ compileExpr left
          right' = unbox (convertTy tRight) $ compileExpr right
compileExpr (Kiff.IfThenElse tau cond thn els) = undefined
compileExpr (Kiff.IntLit _ n) = box MPL.TyInt $ MPL.IntLit n
compileExpr (Kiff.BoolLit _ b) = box MPL.TyBool $ MPL.BoolLit b
compileExpr (Kiff.UnaryMinus _ e) = box MPL.TyInt $ MPL.UnaryMinus e'
    where e' = unbox MPL.TyInt $ compileExpr e
compileExpr (Kiff.Not _ e) = box MPL.TyBool $ MPL.Not e'
    where e' = unbox MPL.TyBool $ compileExpr e

-- box t e = MPL.Box t e
-- unbox t (MPL.Box t' e) | t == t'   = e
--                        | otherwise = error "Internal error: incompatible boxing/unboxing"
-- unbox t e = MPL.Unbox t e

box t e = e
unbox t e = e


compileMetaTy (Kiff.TyPrimitive Kiff.TyInt)   = MPL.MetaInt
compileMetaTy (Kiff.TyPrimitive Kiff.TyBool)  = MPL.MetaBool
compileMetaTy tau@(Kiff.TyFun x y)              = MPL.MetaClass $ map compileMetaTy $ init $ uncurryTy tau
compileMetaTy _                               = MPL.MetaClass [] -- TODO

compileOp :: Kiff.PrimitiveOp -> MPL.PrimitiveOp -- TODO: use the same def in both languages
compileOp Kiff.OpAdd = MPL.OpAdd
compileOp Kiff.OpSub = MPL.OpSub
compileOp Kiff.OpMul = MPL.OpMul
compileOp Kiff.OpDiv = MPL.OpDiv
compileOp Kiff.OpMod = MPL.OpMod
compileOp Kiff.OpEq  = MPL.OpEq
compileOp Kiff.OpAnd = MPL.OpAnd
compileOp Kiff.OpOr  = MPL.OpOr

data CompilerState = CSt { unique :: Int }
type Compile a = State CompilerState a
                       
mkCompilerState = CSt { unique = 0 }    
    
newMetaVarName :: Compile MPL.MetaVarName
newMetaVarName = do st <- get
                    let u = unique st
                        u' = u + 1
                        st' = st{unique = u'}
                    put st'
                    return $ "_p" ++ (show u)

varsAndSpecM :: Kiff.TPat -> Compile ([MPL.MetaVarDecl], MPL.MetaSpecialization)
varsAndSpecM (Kiff.PVar tau var)       = return ([MPL.MetaVarDecl var mty], MPL.MetaVar var)
    where mty = compileMetaTy tau
varsAndSpecM (Kiff.PApp tau con pats)  = do
  (mdecls, mspecs) <- liftM unzip $ mapM varsAndSpecM pats
  return (concat mdecls, MPL.MetaCall con mspecs)  
varsAndSpecM (Kiff.Wildcard tau)       = do
  mv <- newMetaVarName
  varsAndSpecM (Kiff.PVar tau mv)
varsAndSpecM (Kiff.IntPat _ n)         = return ([], MPL.MetaIntLit n)
varsAndSpecM (Kiff.BoolPat _ b)        = return ([], MPL.MetaBoolLit b)

varsAndSpec :: Kiff.TPat -> ([MPL.MetaVarDecl], MPL.MetaSpecialization)
varsAndSpec pat = evalState (varsAndSpecM pat) mkCompilerState
              
uncurryTy (Kiff.TyFun t t')  = (t:uncurryTy t')
uncurryTy t                  = [t]
