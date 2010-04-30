module Metafun.Compiler where

import qualified Language.Kiff.Syntax as Kiff
import Language.Kiff.Typing

import qualified Language.CxxMPL.Syntax as MPL

import Control.Monad.State    

compileDef :: Kiff.TDef -> [MPL.MetaDef]
compileDef (Kiff.TDef tau name _ eqs) = map (compileDefEq name) eqs

compileDefEq name (Kiff.TDefEq tau pats expr) =
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
    
-- compileTy (Kiff.TyPrimitive Kiff.TyInt)   = MPL.TyInt
-- compileTy (Kiff.TyPrimitive Kiff.TyBool)  = MPL.TyBool

compileTy _ = MPL.TyClass

convertTy (Kiff.TyPrimitive Kiff.TyInt) = MPL.TyInt
convertTy (Kiff.TyPrimitive Kiff.TyBool) = MPL.TyBool
                                            
compileTypeDecl :: Kiff.TypeDecl -> [MPL.MetaDecl]
compileTypeDecl (Kiff.DataDecl name tvs cons) = map compileDataCon cons

compileDataCon :: Kiff.DataCon -> MPL.MetaDecl
compileDataCon (Kiff.DataCon name tys) = MPL.MetaDecl name (map compileMetaTy tys)

                                            
compile :: Kiff.TProgram -> MPL.Program
compile (Kiff.TProgram typedecls tdefs) = MPL.Program metadecls metadefs
    where metadecls = mtydecls ++ mvardecls
          metadefs = concatMap compileDef tdefs
                     
          mtydecls = concatMap compileTypeDecl (listdecl:typedecls)
              where listdecl = Kiff.DataDecl "list" ["a"] [nil,cons]
                    nil = Kiff.DataCon "nil" []
                    cons = Kiff.DataCon "cons" [Kiff.TyVar (Kiff.TvName "a"), Kiff.TyList $ Kiff.TyVar (Kiff.TvName "a")]

          mvardecls = map mvardecl tdefs
          mvardecl (Kiff.TDef tau name _ _) = MPL.MetaDecl name (map compileMetaTy tys)
              where tys = init $ uncurryTy tau

uncurryApp :: Kiff.TExpr -> [Kiff.TExpr]
uncurryApp (Kiff.TApp _ f x)  = (uncurryApp f) ++ [x]
uncurryApp expr               = [expr]
                              
compileExpr :: Kiff.TExpr -> MPL.Expr
compileExpr (Kiff.TVar tau var) = MPL.FormalRef var -- error $ unwords ["TVar", var]
compileExpr (Kiff.TCon tau con) = MPL.Cons con [] -- error $ unwords ["TCon", con]
compileExpr e@(Kiff.TApp tau f x) = mpl $ map compileExpr args
    where (fun:args) = uncurryApp e
          mpl = case fun of
                  Kiff.TVar _ var -> MPL.Typename . MPL.Call var
                  Kiff.TCon _ con -> MPL.Cons con
compileExpr (Kiff.TLam tau pats body) = error "Lambdas not supported"
compileExpr (Kiff.TLet tau defs body) = undefined
compileExpr (Kiff.TPrimBinOp tau op left right) = box (convertTy tau) $ MPL.PrimBinOp op' left' right'
    where tLeft = getTy left
          tRight = getTy right
          tRes = tau
          op' = compileOp op
          left' = unbox (convertTy tLeft) $ compileExpr left
          right' = unbox (convertTy tRight) $ compileExpr right
compileExpr (Kiff.TIfThenElse tau cond thn els) = undefined
compileExpr (Kiff.TIntLit n) = box MPL.TyInt $ MPL.IntLit n
compileExpr (Kiff.TBoolLit b) = box MPL.TyBool $ MPL.BoolLit b
compileExpr (Kiff.TUnaryMinus e) = box MPL.TyInt $ MPL.UnaryMinus e'
    where e' = unbox MPL.TyInt $ compileExpr e

box t e = MPL.Box t e
unbox t (MPL.Box t' e) | t == t'   = e
                       | otherwise = error "Internal error: incompatible boxing/unboxing"
unbox t e = MPL.Unbox t e
               
-- compileMetaTy (Kiff.TyPrimitive Kiff.TyInt)   = MPL.MetaInt
-- compileMetaTy (Kiff.TyPrimitive Kiff.TyBool)  = MPL.MetaBool
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
                                                
data DefEqSt = DefEqSt { metavarnum :: Int }
type DE a = State DefEqSt a

mkDefEqSt = DefEqSt { metavarnum = 0 }
    
newMetaVarName :: DE MPL.MetaVarName
newMetaVarName = do st <- get
                    let mvnum = metavarnum st
                        mvnum' = mvnum + 1
                        st' = st{metavarnum = mvnum'}
                    put st'
                    return $ "_p" ++ (show mvnum)

varsAndSpecM :: Kiff.TPat -> DE ([MPL.MetaVarDecl], MPL.MetaSpecialization)
varsAndSpecM (Kiff.TPVar tau var)       = return ([MPL.MetaVarDecl var mty], MPL.MetaVar var)
    where mty = compileMetaTy tau
varsAndSpecM (Kiff.TPApp tau con pats)  = do
  (mdecls, mspecs) <- liftM unzip $ mapM varsAndSpecM pats
  return (concat mdecls, MPL.MetaCall con mspecs)  
varsAndSpecM (Kiff.TWildcard tau)       = do
  mv <- newMetaVarName
  varsAndSpecM (Kiff.TPVar tau mv)
varsAndSpecM (Kiff.TIntPat n)         = return ([], MPL.MetaIntLit n)
varsAndSpecM (Kiff.TBoolPat b)        = return ([], MPL.MetaBoolLit b)

varsAndSpec :: Kiff.TPat -> ([MPL.MetaVarDecl], MPL.MetaSpecialization)
varsAndSpec pat = evalState (varsAndSpecM pat) mkDefEqSt
              
uncurryTy (Kiff.TyFun t t')  = (t:uncurryTy t')
uncurryTy t                  = [t]
