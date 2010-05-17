module Language.Kiff.Typing.Infer (infer) where

import Language.Kiff.Syntax
import Language.Kiff.Typing
import Language.Kiff.Typing.Substitution
import Language.Kiff.Typing.Unify
import Language.Kiff.Typing.Instantiate
import Language.Kiff.Typing.State
import Language.Kiff.CallGraph

import qualified Data.Map as Map
import Data.Either
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
    
getTy :: Tagged e => e Ty -> Ty    
getTy e = getTag e    

infer :: Program () -> Program Ty
infer (Program decls defs) = runTyping $ do
                               (defs', ()) <- withCons cons $ inferDefs defs $ return ()
                               return $ Program decls defs'
    where cons = concatMap toEntries decls
          toEntries (DataDecl name tvs datacons) = map toEntry datacons
              where toEntry (DataCon con tys) = (con, tyFun tys ++ (tyApp (TyCon name):tvs)
        
inferGroup :: [Def ()] -> Typing [Def Ty]
inferGroup defs = do newVars <- mapM mkVar defs
                     (defs', eqs) <- listen $ withMonoVars newVars $ mapM collectDef defs
                     case unify eqs of
                       Left errs -> error (show errs) -- TODO: errors
                       Right s   -> mapM (checkDecl . fmap (subst s)) defs'
                       
    where mkVar (Def _ name _ _) = do tau <- mkTyVar
                                      return (name, tau)
                                             
          checkDecl :: Def Ty -> Typing (Def Ty)
          checkDecl def@(Def tau name Nothing defeqs) = return def
          checkDecl (Def tau name (Just tau') defeqs) =  case fitDecl tau' tau of
                                                       Left errs  -> error (show errs) -- TODO: errors
                                                       Right s    -> return $ Def tau' name (Just tau') (map (fmap $ subst s) defeqs)
          
inferDefs :: [Def ()] -> Typing a -> Typing ([Def Ty], a)
inferDefs defs typing = inferGroups defgroups
    where defgroups :: [[Def ()]]
          defgroups = sortDefs defs

          inferGroups [] = do res <- typing
                              return ([], res)
          inferGroups (g:gs) = do defs' <- inferGroup g
                                  let vars = map (\ (Def tau name _ _) -> (name, tau)) defs'
                                  (defss', res) <- withPolyVars vars $ inferGroups gs
                                  return (defs' ++ defss', res)
                      
collectDef :: Def () -> Typing (Def Ty)
collectDef (Def _ name decl defeqs) = do tau <- mkTyVar
                                         tdefeqs <- mapM collectDefEq defeqs
                                         tell $ map (\ tdefeq -> (tau :=: getTy tdefeq)) tdefeqs
                                         return $ Def tau name decl tdefeqs
              
collectDefEq :: DefEq () -> Typing (DefEq Ty)
collectDefEq (DefEq _ pats body) = do (tpats, tbody) <- inferPats pats $ collectExpr body
                                      let pts = map getTy tpats
                                          bt = getTy tbody
                                          tau = tyFun (pts ++ [bt])
                                      return $ DefEq tau tpats tbody

collectExpr :: Expr () -> Typing (Expr Ty)                                             
collectExpr (Var _ var)                 = do bind <- lookupVar var
                                             case bind of
                                               Nothing -> error $ unwords ["Out of scope reference:", var] -- TODO: errors
                                               Just (Mono tau) -> return $ Var tau var
                                               Just (Poly tau) -> do tau' <- instantiate tau
                                                                     return $ Var tau' var
collectExpr (Con _ con)                 = do tyCon <- lookupCon con
                                             case tyCon of
                                               Nothing -> error $ unwords ["Unknown constructor:", con] -- TODO: errors
                                               Just tau -> do tau' <- instantiate tau
                                                              return $ Con tau' con
collectExpr (App _ f x)                 = do f' <- collectExpr f
                                             x' <- collectExpr x
                                             tau <- mkTyVar
                                             tell [getTy f' :=: TyFun (getTy x') tau]
                                             return $ App tau f' x'                                    
collectExpr (PrimBinOp _ op left right) = do alpha <- mkTyVar
                                             left' <- collectExpr left
                                             right' <- collectExpr right
                                             let tLeft = getTy left'
                                                 tRight = getTy right'
                                                 tau = tyFun [TyPrimitive t1, TyPrimitive t2, TyPrimitive t3]
                                                 tau' = tyFun [tLeft, tRight, alpha]
                                             tell [tau :=: tau']
                                             return $ PrimBinOp alpha op left' right'
    where (t1, t2, t3) = typeOfOp op                       
collectExpr (IfThenElse _ cond thn els) = do cond' <- collectExpr cond
                                             thn' <- collectExpr thn
                                             els' <- collectExpr els
                                             alpha <- mkTyVar
                                             tell [getTy cond' :=: TyPrimitive TyBool, getTy thn' :=: alpha, getTy els' :=: alpha]
                                             return $ IfThenElse alpha cond' thn' els'
collectExpr (IntLit _ n)                = return $ IntLit (TyPrimitive TyInt) n
collectExpr (BoolLit _ b)               = return $ BoolLit (TyPrimitive TyBool) b
collectExpr (UnaryMinus _ e)            = do e' <- collectExpr e
                                             let tau = getTy e'
                                             tell [tau :=: TyPrimitive TyInt]
                                             return $ UnaryMinus (TyPrimitive TyInt) e'
collectExpr (Not _ e)                   = do e' <- collectExpr e
                                             let tau = getTy e'
                                             tell [tau :=: TyPrimitive TyBool]
                                             return $ Not (TyPrimitive TyBool) e'
collectExpr (Lam _ pats body)           = do (pats', body') <- inferPats pats $ collectExpr body
                                             let tPats = map getTy pats'
                                                 tBody = getTy body'
                                             return $ Lam (tyFun (tPats ++ [tBody])) pats' body'
collectExpr (Let _ defs body)           = do (defs', body') <- inferDefs defs $ collectExpr body
                                             let tau = getTy body'
                                             return $ Let tau defs' body'
                                          
collectPats :: [Pat ()] -> Typing ([Pat Ty], [(VarName, Ty)])
collectPats pats = do (tpats, bindss) <- liftM unzip $ mapM collectPat pats
                      let binds = concat bindss -- TODO: Check that pattern variable names are non-overlapping
                      return (tpats, binds)

inferPats :: [Pat ()] -> Typing a -> Typing ([Pat Ty], a)
inferPats pats typing = do (tpats, binds) <- collectPats pats
                           result <- withMonoVars binds typing
                           return (tpats, result)

collectPat :: Pat () -> Typing (Pat Ty, [(VarName, Ty)])
collectPat (Wildcard _)      = do tau <- mkTyVar
                                  return (Wildcard tau, [])
collectPat (IntPat _ n)      = return (IntPat (TyPrimitive TyInt) n, [])
collectPat (BoolPat _ b)     = return (BoolPat (TyPrimitive TyBool) b, [])
collectPat (PVar _ var)      = do tau <- mkTyVar
                                  return (PVar tau var, [(var, tau)])
collectPat (PApp _ con pats) = do tyCon <- lookupCon con
                                  case tyCon of
                                    Nothing -> error $ unwords ["Unknown constructor", con] -- TODO: errors
                                    Just tau -> do (tpats, binds) <- collectPats pats                                                                 
                                                   let ts = map getTy tpats
                                                   alpha <- mkTyVar
                                                   tell [tau :=: tyFun (ts ++ [alpha])]
                                                   return (PApp alpha con tpats, binds)
                                                  
intOp = (TyInt, TyInt, TyInt)                                                                        
intRel = (TyInt, TyInt, TyBool)
boolOp = (TyBool, TyBool, TyBool)
        
typeOfOp OpAdd  = intOp
typeOfOp OpSub  = intOp
typeOfOp OpMul  = intOp
typeOfOp OpMod  = intOp
typeOfOp OpAnd  = boolOp
typeOfOp OpOr   = boolOp
typeOfOp OpEq   = intRel -- TODO
typeOfOp OpNe   = intRel -- TODO
typeOfOp OpGe   = intRel
typeOfOp OpGt   = intRel
typeOfOp OpLt   = intRel
typeOfOp OpLe   = intRel
