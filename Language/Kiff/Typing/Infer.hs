module Language.Kiff.Typing.Infer where

import Language.Kiff.Syntax
import Language.Kiff.Typing
import Language.Kiff.Typing.Substitution
import Language.Kiff.Typing.Unify
import Language.Kiff.Typing.Instantiate
import Language.Kiff.CallGraph

import Debug.Trace    
    
import qualified Data.Map as Map
import Data.Supply
import Data.Either

inferGroup :: Supply TvId -> Ctx -> [Def ()] -> Either [UnificationError] [Def Ty]
inferGroup ids ctx defs = case unify eqs of
                            Left errs  -> Left errs
                            Right s    -> foldl step (Right []) $ zipWith (\ ids tdef -> checkDecl ids (mapTy (xform s) tdef)) (split ids''') tdefs
    where (ids', ids'', ids''') = split3 ids
          (tdefs, eqss) = unzip $ zipWith collect (split ids') defs
          eqs = (zipWith eq newVars tdefs) ++ concat eqss
              where eq (name, tau) tdef = tau :=: getTy tdef
                                    
          collect :: Supply TvId -> Def () -> (Def Ty, [TyEq])
          collect ids def = collectDef ids ctx' def                   

          newVars :: [(VarName, Ty)]
          newVars = zipWith mkVar (split ids'') defs
              where mkVar ids (Def _ name _ _) = (name, tau)
                        where tau = mkTv ids
          ctx' = foldl addMonoVar ctx newVars          

          checkDecl :: Supply TvId -> Def Ty -> Either [UnificationError] (Def Ty)
          checkDecl ids tdef@(Def tau name decl tdefeqs) = case decl of
                                                           Nothing  -> Right tdef
                                                           Just tau'  -> case fitDecl tau' tau of
                                                                         Left errs  -> Left errs
                                                                         Right s    -> Right $ Def tau' name (Just tau') (map (mapTy $ xform s) tdefeqs)

          step :: Either [UnificationError] [Def Ty] -> Either [UnificationError] (Def Ty) -> Either [UnificationError] [Def Ty]
          step (Left errs)    (Left errs')  = Left $ errs' ++ errs
          step (Left errs)    (Right _)     = Left $ errs
          step (Right defs)   (Left errs')  = Left $ errs'
          step (Right defs)   (Right def)  = Right $ defs ++ [def]

inferDefs :: Supply TvId -> Ctx -> [Def ()] -> Either [UnificationError] (Ctx, [Def Ty])
inferDefs ids ctx defs = foldl step (Right (ctx, [])) $ zip (split ids) defgroups
    where defgroups :: [[Def ()]]
          defgroups = sortDefs defs

          step :: Either [UnificationError] (Ctx, [Def Ty]) -> (Supply TvId, [Def ()]) -> Either [UnificationError] (Ctx, [Def Ty])
          step (Left errs)           (ids, defs)  = Left errs -- TODO: collect more errors
          step (Right (ctx, tdefs))  (ids, defs)  = case inferGroup ids ctx defs of
                                                      Left errs     -> Left errs
                                                      Right tdefs'  -> Right (foldl addDef ctx tdefs', tdefs ++ tdefs')

          addDef :: Ctx -> Def Ty -> Ctx
          addDef ctx (Def tau name _ _) = addPolyVar ctx (name, tau)

collectDef :: Supply TvId -> Ctx -> Def () -> (Def Ty, [TyEq])
collectDef ids ctx (Def _ name decl defeqs) = (Def tau name decl tdefeqs, eqs)
    where tau = mkTv ids
          (tdefeqs, eqss) = unzip $ zipWith collect (split ids) defeqs
          collect ids defeq = collectDefEq ids ctx defeq
          eqs = (map (\ tdefeq -> (tau :=: (getTy tdefeq))) tdefeqs) ++ (concat eqss)

                                         
collectDefEq :: Supply TvId -> Ctx -> DefEq () -> (DefEq Ty, [TyEq])
collectDefEq ids ctx (DefEq _ pats body) = (DefEq tau tpats tbody, peqs ++ beqs)
    where (ids', ids'') = split2 ids
          (tpats, ctx', peqs) = inferPats ids'' ctx pats
          (tbody, beqs) = collectExpr ids' ctx' body
          tau = tyFun (pts ++ [bt])
          pts = map getTy tpats
          bt = getTy tbody                                    
          
mkTv :: Supply TvId -> Ty
mkTv ids = TyVar $ TvId $ supplyValue ids
               
collectExpr :: Supply TvId -> Ctx -> Expr () -> (Expr Ty, [TyEq])
collectExpr ids ctx (Var _ var) = case lookupVar ctx var of
                                    Just (Mono tau) -> (Var tau var, [])
                                    Just (Poly tau) -> (Var tau' var, [])
                                        where tau' = instantiate ids ctx tau
                                    Nothing -> error $ unlines [unwords ["Out of scope reference:", var], show ctx]
collectExpr ids ctx (Con _ con) = case Map.lookup con (conmap ctx) of
                                    Just tau -> (Con tau' con, [])
                                        where tau' = instantiate ids ctx tau
collectExpr ids ctx (App _ f x) = (App tau tf tx, (ft :=: TyFun xt tau):(feqs ++ xeqs))
    where  (ids', ids'') = split2 ids
           (tf, feqs) = collectExpr ids' ctx f
           (tx, xeqs) = collectExpr ids'' ctx x
           ft = getTy tf
           xt = getTy tx
           tau = mkTv ids
collectExpr ids ctx (PrimBinOp _ op left right) = (PrimBinOp alpha op tleft tright, (tau :=: tau'):(leqs ++ reqs))
    where  (ids', ids'') = split2 ids
           (tleft, leqs) = collectExpr ids' ctx left
           (tright, reqs) = collectExpr ids'' ctx right
           lt = getTy tleft
           rt = getTy tright
           (t1, t2, t3) = typeOfOp op
           tau = tyFun  [TyPrimitive t1, TyPrimitive t2, TyPrimitive t3]
           alpha = mkTv ids
           tau' = tyFun [lt, rt, alpha]               
collectExpr ids ctx (IfThenElse _ cond thn els) = (IfThenElse alpha cond' thn' els', eqs++ceqs++teqs++eeqs)
    where  (ids', ids'', ids3) = split3 ids
           (ct, tt, et) = (getTy cond', getTy thn', getTy els')
           (cond', ceqs) = collectExpr ids' ctx cond
           (thn', teqs) = collectExpr ids'' ctx thn
           (els', eeqs) = collectExpr ids3 ctx els
           alpha = mkTv ids
           eqs = [tt :=: alpha, et :=: alpha, ct :=: TyPrimitive TyBool]               
collectExpr ids ctx (IntLit _ n) = (IntLit (TyPrimitive TyInt) n, [])
collectExpr ids ctx (BoolLit _ b) = (BoolLit (TyPrimitive TyBool) b, [])
collectExpr ids ctx (UnaryMinus _ e) = (UnaryMinus (TyPrimitive TyInt) e', (TyPrimitive TyInt :=: tau):eqs)
    where  (e', eqs) = collectExpr ids ctx e           
           tau = getTy e'
collectExpr ids ctx (Not _ e) = (Not (TyPrimitive TyBool) e', (TyPrimitive TyBool :=: tau):eqs)
    where (e', eqs) = collectExpr ids ctx e
          tau = getTy e'
collectExpr ids ctx (Lam _ pats body) = (Lam (tyFun (pts ++ [tau])) tpats tbody, peqs ++ eqs)
    where  (ids', ids'') = split2 ids
           (tpats, ctx', peqs) = inferPats ids' ctx pats
           pts = map getTy tpats
           (tbody, eqs) = collectExpr ids'' ctx' body
           tau = getTy tbody
collectExpr ids ctx (Let _ defs body) = (Let tau tdefs tbody, eqs)
    where (ids', ids'') = split2 ids
          (tbody, eqs) = collectExpr ids' ctx' body
          tau = getTy tbody
          (ctx', tdefs) = case inferDefs ids'' ctx defs of
                   Right (ctx', tdefs) -> (ctx', tdefs) -- TODO: error handling

collectPats :: Supply TvId -> Ctx -> [Pat ()] -> ([Pat Ty], [(VarName, Ty)], [TyEq])
collectPats ids ctx pats = (tpats, binds, eqs)
    where (tpats, bindss, eqss) = unzip3 $ zipWith collect (split ids) pats
          collect ids pat = collectPat ids ctx pat
          binds = concat bindss -- TODO: Check that pattern variable names are unique
          eqs = concat eqss

inferPats :: Supply TvId -> Ctx -> [Pat ()] -> ([Pat Ty], Ctx, [TyEq])
inferPats ids ctx pats = (tpats, ctx', eqs)
    where (tpats, binds, eqs) = collectPats ids ctx pats
          ctx' = foldl addMonoVar ctx binds

                 
collectPat :: Supply TvId -> Ctx -> Pat () -> (Pat Ty, [(VarName, Ty)], [TyEq])
collectPat ids ctx (Wildcard _)      = (Wildcard tau, [], [])
    where tau = mkTv ids
collectPat ids ctx (IntPat _ n)      = (IntPat (TyPrimitive TyInt) n, [], [])
collectPat ids ctx (BoolPat _ b)     = (BoolPat (TyPrimitive TyBool) b, [], [])
collectPat ids ctx (PVar _ var)      = (PVar tau var, [(var, tau)], [])
    where tau = mkTv ids
collectPat ids ctx (PApp _ con pats)  = (PApp alpha con tpats, binds, (tau :=: tyFun (ts ++ [alpha])):eqs)
    where   (ids', ids'') = split2 ids
            tau = case Map.lookup con (conmap ctx) of
                  Just t -> instantiate ids' ctx t
            (tpats, binds, eqs) = collectPats ids'' ctx pats
            ts = map getTy tpats
            alpha = mkTv ids
                
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
