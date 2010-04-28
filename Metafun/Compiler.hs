module Metafun.Compiler where

import qualified Language.Kiff.Syntax as Kiff
import Language.Kiff.Typing.Infer

import qualified Language.CxxMPL.Syntax as MPL

import Control.Monad.State    

compileDef :: Kiff.TDef -> [MPL.MetaDef]
compileDef (Kiff.TDef tau name _ eqs) = map (compileDefEq name) eqs

compileDefEq name (Kiff.TDefEq tau pats expr) =
    MPL.MetaDef { MPL.mdefName = name,
                  MPL.mdefFormals = formals,
                  MPL.mdefSpec = specs,
                  MPL.mdefFields = [],
                  MPL.mdefBody = (compileTy tyRet, compileExpr expr) }
        where tyRet = last $ uncurryTy tau
              (formalss, specs) = unzip $ map varsAndSpec pats
              formals = concat formalss
    
compileTy (Kiff.TyPrimitive Kiff.TyInt)   = MPL.TyInt
compileTy (Kiff.TyPrimitive Kiff.TyBool)  = MPL.TyBool
                                            
compile :: Kiff.TProgram -> MPL.Program
compile (Kiff.TProgram typedecls tdefs) = MPL.Program metadecls metadefs
    where metadecls = mtydecls ++ mvardecls
          metadefs = concatMap compileDef tdefs
                     
          mtydecls = concatMap mtydecl typedecls
          mtydecl (Kiff.DataDecl name tvs cons) = map mtycon cons
          mtycon (Kiff.DataCon name tys) = MPL.MetaDecl name (map compileMetaTy tys)

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
                  Kiff.TVar _ var -> MPL.Call var
                  Kiff.TCon _ con -> MPL.Cons con
compileExpr (Kiff.TLam tau pats body) = undefined
compileExpr (Kiff.TLet tau defs body) = undefined
compileExpr (Kiff.TPrimBinOp tau op left right) = MPL.PrimBinOp op' left' right'
    where op' = compileOp op
          left' = compileExpr left
          right' = compileExpr right
compileExpr (Kiff.TIfThenElse tau cond thn els) = undefined
compileExpr (Kiff.TIntLit n) = MPL.IntLit n
compileExpr (Kiff.TBoolLit b) = MPL.BoolLit b
compileExpr (Kiff.TUnaryMinus e) = MPL.UnaryMinus (compileExpr e)
                              
compileMetaTy (Kiff.TyPrimitive Kiff.TyInt)   = MPL.MetaInt
compileMetaTy (Kiff.TyPrimitive Kiff.TyBool)  = MPL.MetaBool
compileMetaTy _                               = MPL.MetaTypename -- TODO

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
