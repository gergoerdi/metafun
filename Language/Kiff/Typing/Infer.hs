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

inferGroup :: Supply TvId -> Ctx -> [Def] -> Either [UnificationError] [TDef]
inferGroup ids ctx defs = case unify eqs of
                            Left errs  -> Left errs
                            Right s    -> foldl step (Right []) $ zipWith (\ ids tdef -> checkDecl ids (mapTy (xform s) tdef)) (split ids''') tdefs
    where (ids', ids'', ids''') = split3 ids
          (tdefs, eqss) = unzip $ zipWith collect (split ids') defs
          eqs = (zipWith eq newVars tdefs) ++ concat eqss
              where eq (name, tau) tdef = tau :=: getTy tdef
                                    
          collect :: Supply TvId -> Def -> (TDef, [TyEq])
          collect ids def = collectDef ids ctx' def                   

          newVars :: [(VarName, Ty)]
          newVars = zipWith mkVar (split ids'') defs
              where mkVar ids (Def name _ _) = (name, tau)
                        where tau = mkTv ids
          ctx' = foldl addMonoVar ctx newVars          

          checkDecl :: Supply TvId -> TDef -> Either [UnificationError] TDef
          checkDecl ids tdef@(TDef tau name decl tdefeqs) = case decl of
                                                            Nothing  -> Right tdef
                                                            Just tau'  -> case fitDecl tau' tau of
                                                                          Left errs  -> Left errs
                                                                          Right s    -> Right $ TDef tau' name (Just tau') (map (mapTy $ xform s) tdefeqs)

          step :: Either [UnificationError] [TDef] -> Either [UnificationError] TDef -> Either [UnificationError] [TDef]
          step (Left errs)    (Left errs')  = Left $ errs' ++ errs
          step (Left errs)    (Right _)     = Left $ errs
          step (Right tdefs)  (Left errs')  = Left $ errs'
          step (Right tdefs)  (Right tdef)  = Right $ tdefs ++ [tdef]

inferDefs :: Supply TvId -> Ctx -> [Def] -> Either [UnificationError] (Ctx, [TDef])
inferDefs ids ctx defs = trace (show $ map (map (\ (Def name _ _) -> name)) defgroups) $ foldl step (Right (ctx, [])) $ zip (split ids) defgroups
    where defgroups :: [[Def]]
          defgroups = sortDefs defs

          step :: Either [UnificationError] (Ctx, [TDef]) -> (Supply TvId, [Def]) -> Either [UnificationError] (Ctx, [TDef])
          step (Left errs)           (ids, defs)  = Left errs -- TODO: collect more errors
          step (Right (ctx, tdefs))  (ids, defs)  = case inferGroup ids ctx defs of
                                                      Left errs     -> Left errs
                                                      Right tdefs'  -> Right (foldl addDef ctx tdefs', tdefs ++ tdefs')

          addDef :: Ctx -> TDef -> Ctx
          addDef ctx (TDef tau name _ _) = addPolyVar ctx (name, tau)

collectDef :: Supply TvId -> Ctx -> Def -> (TDef, [TyEq])
collectDef ids ctx (Def name decl defeqs) = (TDef tau name decl tdefeqs, eqs)
    where tau = mkTv ids
          (tdefeqs, eqss) = unzip $ zipWith collect (split ids) defeqs
          collect ids defeq = collectDefEq ids ctx defeq
          eqs = (map (\ tdefeq -> (tau :=: (getTy tdefeq))) tdefeqs) ++ (concat eqss)

                                         
collectDefEq :: Supply TvId -> Ctx -> DefEq -> (TDefEq, [TyEq])
collectDefEq ids ctx (DefEq pats body) = (TDefEq tau tpats tbody, peqs ++ beqs)
    where (ids', ids'') = split2 ids
          (tpats, ctx', peqs) = inferPats ids'' ctx pats
          (tbody, beqs) = collectExpr ids' ctx' body
          tau = tyFun (pts ++ [bt])
          pts = map getTy tpats
          bt = getTy tbody                                    
          
mkTv :: Supply TvId -> Ty
mkTv ids = TyVar $ TvId $ supplyValue ids
               
collectExpr :: Supply TvId -> Ctx -> Expr -> (TExpr, [TyEq])
collectExpr ids ctx (Var var) = case lookupVar ctx var of
                                  Just (Mono tau) -> (TVar tau var, [])
                                  Just (Poly tau) -> (TVar tau' var, [])
                                      where tau' = instantiate ids ctx tau
                                  Nothing -> error $ unlines [unwords ["Out of scope reference:", var], show ctx]
collectExpr ids ctx (Con con) = case Map.lookup con (conmap ctx) of
                                  Just tau -> (TCon tau' con, [])
                                      where tau' = instantiate ids ctx tau
collectExpr ids ctx (App f x) = (TApp tau tf tx, (ft :=: TyFun xt tau):(feqs ++ xeqs))
    where  (ids', ids'') = split2 ids
           (tf, feqs) = collectExpr ids' ctx f
           (tx, xeqs) = collectExpr ids'' ctx x
           ft = getTy tf
           xt = getTy tx
           tau = mkTv ids
collectExpr ids ctx (PrimBinOp op left right) = (TPrimBinOp alpha op tleft tright, (tau :=: tau'):(leqs ++ reqs))
    where  (ids', ids'') = split2 ids
           (tleft, leqs) = collectExpr ids' ctx left
           (tright, reqs) = collectExpr ids'' ctx right
           lt = getTy tleft
           rt = getTy tright
           (t1, t2, t3) = typeOfOp op
           tau = tyFun  [TyPrimitive t1, TyPrimitive t2, TyPrimitive t3]
           alpha = mkTv ids
           tau' = tyFun [lt, rt, alpha]               
collectExpr ids ctx (IfThenElse cond thn els) = (TIfThenElse alpha cond' thn' els', eqs++ceqs++teqs++eeqs)
    where  (ids', ids'', ids3) = split3 ids
           (ct, tt, et) = (getTy cond', getTy thn', getTy els')
           (cond', ceqs) = collectExpr ids' ctx cond
           (thn', teqs) = collectExpr ids'' ctx thn
           (els', eeqs) = collectExpr ids3 ctx els
           alpha = mkTv ids
           eqs = [tt :=: alpha, et :=: alpha, ct :=: TyPrimitive TyBool]               
collectExpr ids ctx (IntLit n) = (TIntLit n, [])
collectExpr ids ctx (BoolLit b) = (TBoolLit b, [])
collectExpr ids ctx (UnaryMinus e) = (TUnaryMinus e', (TyPrimitive TyInt :=: tau):eqs)
    where  (e', eqs) = collectExpr ids ctx e
           tau = getTy e'
collectExpr ids ctx (Lam pats body) = (TLam (tyFun (pts ++ [tau])) tpats tbody, peqs ++ eqs)
    where  (ids', ids'') = split2 ids
           (tpats, ctx', peqs) = inferPats ids' ctx pats
           pts = map getTy tpats
           (tbody, eqs) = collectExpr ids'' ctx' body
           tau = getTy tbody
collectExpr ids ctx (Let defs body) = (TLet tau tdefs tbody, eqs)
    where (ids', ids'') = split2 ids
          (tbody, eqs) = collectExpr ids' ctx' body
          tau = getTy tbody
          (ctx', tdefs) = case inferDefs ids'' ctx defs of
                   Right (ctx', tdefs) -> (ctx', tdefs) -- TODO: error handling

collectPats :: Supply TvId -> Ctx -> [Pat] -> ([TPat], [(VarName, Ty)], [TyEq])
collectPats ids ctx pats = (tpats, binds, eqs)
    where (tpats, bindss, eqss) = unzip3 $ zipWith collect (split ids) pats
          collect ids pat = collectPat ids ctx pat
          binds = concat bindss -- TODO: Check that pattern variable names are unique
          eqs = concat eqss

inferPats :: Supply TvId -> Ctx -> [Pat] -> ([TPat], Ctx, [TyEq])
inferPats ids ctx pats = (tpats, ctx', eqs)
    where (tpats, binds, eqs) = collectPats ids ctx pats
          ctx' = foldl addMonoVar ctx binds

                 
collectPat :: Supply TvId -> Ctx -> Pat -> (TPat, [(VarName, Ty)], [TyEq])
collectPat ids ctx Wildcard         = (TWildcard tau, [], [])
    where tau = mkTv ids
collectPat ids ctx (IntPat n)       = (TIntPat n, [], [])
collectPat ids ctx (BoolPat b)      = (TBoolPat b, [], [])
collectPat ids ctx (PVar var)       = (TPVar tau var, [(var, tau)], [])
    where tau = mkTv ids
collectPat ids ctx (PApp con pats)  = (TPApp alpha con tpats, binds, (tau :=: tyFun (ts ++ [alpha])):eqs)
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
