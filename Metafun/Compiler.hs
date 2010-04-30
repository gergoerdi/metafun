module Metafun.Compiler where

import qualified Language.Kiff.Syntax as Kiff
import Language.Kiff.Typing
import qualified Language.Kiff.Typing.Substitution as Subst
    
import qualified Language.CxxMPL.Syntax as MPL

import Metafun.Compiler.State
import Control.Monad
import Data.List
    
import Debug.Trace
    
compileDef :: Kiff.TDef -> Compile [MPL.MetaDef]
compileDef (Kiff.Def tau name _ eqs) = mapM (compileDefEq name) eqs

compileDefEq :: Kiff.VarName -> Kiff.TDefEq -> Compile MPL.MetaDef
compileDefEq name (Kiff.DefEq tau pats expr) = do
  (formalss, specs) <- liftM unzip $ mapM varsAndSpec pats
  let specs' = if (all isVar specs) then Nothing else Just specs
          where isVar (MPL.MetaVar _) = True
                isVar _               = False
      formals = concat formalss
      tyRet = last $ uncurryTy tau
  tyBody <- compileTy tyRet
  body <- compileExpr expr        
  return $ MPL.MetaDef { MPL.mdefName = name,
                         MPL.mdefFormals = formals,
                         MPL.mdefSpec = specs',
                         MPL.mdefFields = [],
                         MPL.mdefBody = (tyBody, body) }
    
compileTy (Kiff.TyPrimitive Kiff.TyInt)  = return $ MPL.TyInt
compileTy (Kiff.TyPrimitive Kiff.TyBool) = return $ MPL.TyBool
compileTy _ = return $ MPL.TyClass

convertTy (Kiff.TyPrimitive Kiff.TyInt)  = MPL.TyInt
convertTy (Kiff.TyPrimitive Kiff.TyBool) = MPL.TyBool

specializeConName :: Kiff.ConName -> [Kiff.Ty] -> Kiff.ConName                                           
specializeConName name specs = concat $ intersperse "_" $ name:(map tag specs)
    where tag (Kiff.TyPrimitive Kiff.TyInt)  = "Int"
          tag (Kiff.TyPrimitive Kiff.TyBool) = "Bool"
          tag _                              = "_"

compileTypeDecl :: Kiff.TypeDecl -> Compile [MPL.MetaDecl]
compileTypeDecl (Kiff.DataDecl name tvnames cons) = do
  mapM compileDataCon $ concatMap specialize cons
    where tvs = map Kiff.TvName tvnames
          specialize con@(Kiff.DataCon name tys) = map specializeAs spectys
              where specializeAs specs = Kiff.DataCon name' tys'
                        where name' = specializeConName name $ zipWith helper specs tvs
                                  where helper (Just ty) _  = ty
                                        helper Nothing   tv = Kiff.TyVar tv
                                              
                              subst = foldl step Subst.empty $ zip specs tvs
                              step s (Nothing, _)    = s
                              step s ((Just ty), tv) = Subst.add s tv ty

                              tys' = map (Subst.xform subst) tys
                                      
          spectysOf tv = [Nothing, Just $ Kiff.TyPrimitive Kiff.TyInt, Just $ Kiff.TyPrimitive Kiff.TyBool]
              where ty = Kiff.TyVar tv
          spectys = combinations $ map spectysOf tvs
                    
combinations :: [[a]] -> [[a]]
combinations []          = [[]]
combinations ([]:_)      = []
combinations ((x:xs):ys) = (map (x:) (combinations ys)) ++ (combinations (xs:ys))
                           

compileDataCon :: Kiff.DataCon -> Compile MPL.MetaDecl
compileDataCon (Kiff.DataCon name tys) = do
  mtys <- mapM compileMetaTy tys
  return $ MPL.MetaDecl name mtys

                                            
compile :: Kiff.TProgram -> Compile MPL.Program
compile (Kiff.Program typedecls defs) = do
  mtydecls <- concatMapM compileTypeDecl (listdecl:typedecls)
  mvardecls <- mapM mvardecl defs
  metadefs <- concatMapM compileDef defs
  let metadecls = mtydecls ++ mvardecls
  return $ MPL.Program metadecls metadefs
      where listdecl = Kiff.DataDecl "list" ["a"] [nil,cons]
                where nil = Kiff.DataCon "nil" []
                      cons = Kiff.DataCon "cons" [Kiff.TyVar (Kiff.TvName "a"), Kiff.TyList $ Kiff.TyVar (Kiff.TvName "a")]

            mvardecl (Kiff.Def tau name _ _) = liftM (MPL.MetaDecl name) $ mapM compileMetaTy tys
              where tys = init $ uncurryTy tau

uncurryApp :: Kiff.TExpr -> [Kiff.TExpr]
uncurryApp (Kiff.App _ f x) = (uncurryApp f) ++ [x]
uncurryApp expr             = [expr]                              

callCon :: Kiff.ConName -> Kiff.Ty -> Kiff.ConName
callCon con tau = specializeConName con args
    where args = case t of
                   Kiff.TyList tElem -> [tElem]
                   _ -> tail $ uncurryTyApp t
          t = last $ uncurryTy tau
                              
compileExpr :: Kiff.TExpr -> Compile MPL.Expr
compileExpr (Kiff.Var tau var) = return $ MPL.FormalRef var
compileExpr (Kiff.Con tau con) = return $ MPL.Cons (callCon con tau) []
compileExpr e@(Kiff.App tau f x) = do
  let (fun:args) = uncurryApp e
      mpl = case fun of
              Kiff.Var _ var -> case tau of
                                  Kiff.TyPrimitive _  -> MPL.Call var
                                  _                   -> MPL.Typename . MPL.Call var
              Kiff.Con tau con -> MPL.Cons (callCon con tau)
  args' <- mapM compileExpr args
  return $ mpl args'
compileExpr (Kiff.Lam tau pats body) = error "Lambdas not supported"
compileExpr (Kiff.Let tau defs body) = undefined
compileExpr (Kiff.PrimBinOp tau op left right) = do
  left' <- compileExpr left
  right' <- compileExpr right
  let op' = compileOp op
  return $ MPL.PrimBinOp op' left' right'
compileExpr (Kiff.IfThenElse tau cond thn els) = undefined
compileExpr (Kiff.IntLit _ n) = return $ MPL.IntLit n
compileExpr (Kiff.BoolLit _ b) = return $ MPL.BoolLit b
compileExpr (Kiff.UnaryMinus _ e) = do
  e' <- compileExpr e
  return $ MPL.UnaryMinus e'
compileExpr (Kiff.Not _ e) = do
  e' <- compileExpr e      
  return $ MPL.Not e'

compileMetaTy :: Kiff.Ty -> Compile MPL.MetaTy
compileMetaTy (Kiff.TyPrimitive Kiff.TyInt)   = return MPL.MetaInt
compileMetaTy (Kiff.TyPrimitive Kiff.TyBool)  = return MPL.MetaBool
compileMetaTy tau@(Kiff.TyFun x y)              = do
  mtys <- mapM compileMetaTy $ init $ uncurryTy tau
  return $ MPL.MetaClass mtys
compileMetaTy _                               = return $ MPL.MetaClass [] -- TODO

compileOp :: Kiff.PrimitiveOp -> MPL.PrimitiveOp -- TODO: use the same def in both languages
compileOp Kiff.OpAdd = MPL.OpAdd
compileOp Kiff.OpSub = MPL.OpSub
compileOp Kiff.OpMul = MPL.OpMul
compileOp Kiff.OpDiv = MPL.OpDiv
compileOp Kiff.OpMod = MPL.OpMod
compileOp Kiff.OpEq  = MPL.OpEq
compileOp Kiff.OpAnd = MPL.OpAnd
compileOp Kiff.OpOr  = MPL.OpOr

varsAndSpec :: Kiff.TPat -> Compile ([MPL.MetaVarDecl], MPL.MetaSpecialization)
varsAndSpec (Kiff.PVar tau var)       = do
  mty <- compileMetaTy tau
  return ([MPL.MetaVarDecl var mty], MPL.MetaVar var)
varsAndSpec (Kiff.PApp tau con pats)  = do
  (mdecls, mspecs) <- liftM unzip $ mapM varsAndSpec pats
  return (concat mdecls, MPL.MetaCall con' mspecs)
      where con' = callCon con tau
varsAndSpec (Kiff.Wildcard tau)       = do
  mv <- newMetaVarName
  varsAndSpec (Kiff.PVar tau mv)
varsAndSpec (Kiff.IntPat _ n)         = return ([], MPL.MetaIntLit n)
varsAndSpec (Kiff.BoolPat _ b)        = return ([], MPL.MetaBoolLit b)

uncurryTy (Kiff.TyFun t t')  = (t:uncurryTy t')
uncurryTy t                  = [t]

uncurryTyApp (Kiff.TyApp t t') = (uncurryTyApp t) ++ [t']
uncurryTyApp t                 = [t]
