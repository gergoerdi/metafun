module Language.Kiff.Typing.Infer where

import Language.Kiff.Syntax
import Language.Kiff.Typing.Substitution
import Language.Kiff.Typing.Unify
import Language.Kiff.Typing.Instantiate
import Language.Kiff.CallGraph

import Debug.Trace    
    
import qualified Data.Map as Map
import Data.Supply
import Data.Either

class Typed a where
    getTy :: a -> Ty
    mapTy :: (Ty -> Ty) -> a -> a

instance Typed TDef where
    getTy (TDef tau name decl tdefeqs) = tau
    mapTy f (TDef tau name decl tdefeqs) = TDef tau' name decl tdefeqs'
        where tau' = f tau
              tdefeqs' = map (mapTy f) tdefeqs

instance Typed TDefEq where
    getTy (TDefEq tau tpats tbody) = tau
    mapTy f (TDefEq tau tpats tbody) = TDefEq tau' tpats' tbody'
        where tau' = f tau
              tpats' = map (mapTy f) tpats
              tbody' = mapTy f tbody

instance Typed TPat where
    getTy (TPVar tau _)    = tau
    getTy (TPApp tau _ _)  = tau
    getTy (TWildcard tau)  = tau
    getTy (TIntPat _)    = TyPrimitive TyInt
    getTy (TBoolPat _)   = TyPrimitive TyBool

    mapTy f (TPVar tau var)        = TPVar (f tau) var
    mapTy f (TPApp tau con tpats)  = TPApp (f tau) con (map (mapTy f) tpats)
    mapTy f (TWildcard tau)        = TWildcard (f tau)
    mapTy f (TIntPat n)          = TIntPat n
    mapTy f (TBoolPat b)         = TBoolPat b

instance Typed TExpr where
    getTy (TVar tau _)             = tau
    getTy (TCon tau _)             = tau
    getTy (TApp tau _ _)           = tau
    getTy (TLam tau _ _)           = tau
    getTy (TLet tau _ _)           = tau
    getTy (TPrimBinOp tau _ _ _)   = tau
    getTy (TIfThenElse tau _ _ _)  = tau
    getTy (TIntLit _)            = TyPrimitive TyInt
    getTy (TBoolLit _)           = TyPrimitive TyBool
    getTy (TUnaryMinus _)        = TyPrimitive TyInt
                                   
    mapTy f (TVar tau var)                  = TVar (f tau) var
    mapTy f (TCon tau con)                  = TCon (f tau) con
    mapTy f (TApp tau fun x)                = TApp (f tau) (mapTy f fun) (mapTy f x)
    mapTy f (TLam tau pats body)            = TLam (f tau) (map (mapTy f) pats) (mapTy f body)
    mapTy f (TLet tau defs body)            = TLet (f tau) (map (mapTy f) defs) (mapTy f body)
    mapTy f (TPrimBinOp tau op left right)  = TPrimBinOp (f tau) op (mapTy f left) (mapTy f right)
    mapTy f (TIfThenElse tau cond thn els)  = TIfThenElse (f tau) (mapTy f cond) (mapTy f thn) (mapTy f els)
    mapTy f (TIntLit n)                   = TIntLit n
    mapTy f (TBoolLit b)                  = TBoolLit b
    mapTy f (TUnaryMinus expr)            = TUnaryMinus (mapTy f expr)
                                   
data VarBind  = Mono Ty
              | Poly Ty
              deriving Show
    
data Ctx = Ctx { conmap :: Map.Map DataName Ty,
                 varmap :: Map.Map VarName VarBind
               }
           deriving Show

lookupVar :: Ctx -> VarName -> Maybe VarBind
lookupVar ctx v = Map.lookup v (varmap ctx)
                    
tyFun :: [Ty] -> Ty
tyFun = foldr1 TyFun

tyApp :: [Ty] -> Ty
tyApp = foldr1 TyApp
         
mkCtx :: Supply TvId -> Ctx
mkCtx ids = Ctx { conmap = Map.fromList [("nil", tyList),
                                         ("cons", tyFun [TyVarId i, tyList, tyList])],
                  varmap = Map.fromList [("not",  (Poly $ tyFun [TyPrimitive TyBool, TyPrimitive TyBool]))]
                }
    where i = supplyValue ids
          tyList = TyList (TyVarId i)

addVar :: Ctx -> (VarName, VarBind) -> Ctx
addVar ctx@Ctx{varmap = varmap} (name, bind) = ctx{varmap = varmap'}
    where varmap' = Map.insert name bind varmap
                   
addMonoVar :: Ctx -> (VarName, Ty) -> Ctx
addMonoVar ctx (name, ty) = addVar ctx (name, (Mono ty))

addPolyVar :: Ctx -> (VarName, Ty) -> Ctx
addPolyVar ctx (name, ty) = addVar ctx (name, (Poly ty))

inferGroup :: Supply TvId -> Ctx -> [Def] -> Either [UnificationError] [TDef]
inferGroup ids ctx defs = case unify eqs of
                            Left errs  -> Left errs
                            Right s    -> foldl step (Right []) $ map (fitDecl . (mapTy $ xform s)) tdefs
    where (ids', ids'') = split2 ids
          (tdefs, eqss) = unzip $ zipWith collect (split ids') defs
          eqs = (zipWith eq newVars tdefs) ++ concat eqss
              where eq (name, tau) tdef = tau :=: getTy tdef
                                    
          collect :: Supply TvId -> Def -> (TDef, [TyEq])
          collect ids def = collectDef ids ctx' def                   

          newVars :: [(VarName, Ty)]
          newVars = zipWith mkVar (split ids'') defs
              where mkVar ids (Def name _ _) = (name, tau)
                        where tau = TyVarId $ supplyValue ids
          ctx' = foldl addMonoVar ctx newVars

          fitDecl :: TDef -> Either [UnificationError] TDef
          fitDecl tdef@(TDef tau name decl tdefeqs) = case decl of
                                                      Nothing  -> Right tdef
                                                      Just tau'  -> case checkDecl tau' tau of
                                                                    Left errs  -> Left errs
                                                                    Right s    -> Right $ TDef tau' name decl (map (mapTy $ xform s) tdefeqs)

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
    where tau = TyVarId $ supplyValue ids
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
          
                                   
collectExpr :: Supply TvId -> Ctx -> Expr -> (TExpr, [TyEq])
collectExpr ids ctx (Var var) = case lookupVar ctx var of
                                  Just (Mono tau) -> (TVar tau var, [])
                                  Just (Poly tau) -> (TVar tau' var, [])
                                      where tau' = instantiate ids tau
                                  Nothing -> error $ unlines [unwords ["Out of scope reference:", var], show ctx]
collectExpr ids ctx (Con con) = case Map.lookup con (conmap ctx) of
                                  Just tau -> (TCon tau' con, [])
                                      where tau' = instantiate ids tau
collectExpr ids ctx (App f x) = (TApp tau tf tx, (ft :=: TyFun xt tau):(feqs ++ xeqs))
    where  (ids', ids'') = split2 ids
           (tf, feqs) = collectExpr ids' ctx f
           (tx, xeqs) = collectExpr ids'' ctx x
           ft = getTy tf
           xt = getTy tx
           tau = TyVarId $ supplyValue ids              
collectExpr ids ctx (PrimBinOp op left right) = (TPrimBinOp alpha op tleft tright, (tau :=: tau'):(leqs ++ reqs))
    where  (ids', ids'') = split2 ids
           (tleft, leqs) = collectExpr ids' ctx left
           (tright, reqs) = collectExpr ids'' ctx right
           lt = getTy tleft
           rt = getTy tright
           (t1, t2, t3) = typeOfOp op
           tau = tyFun  [TyPrimitive t1, TyPrimitive t2, TyPrimitive t3]
           alpha = TyVarId $ supplyValue ids
           tau' = tyFun [lt, rt, alpha]               
collectExpr ids ctx (IfThenElse cond thn els) = (TIfThenElse alpha cond' thn' els', eqs++ceqs++teqs++eeqs)
    where  (ids', ids'', ids3) = split3 ids
           (ct, tt, et) = (getTy cond', getTy thn', getTy els')
           (cond', ceqs) = collectExpr ids' ctx cond
           (thn', teqs) = collectExpr ids'' ctx thn
           (els', eeqs) = collectExpr ids3 ctx els
           alpha = TyVarId $ supplyValue ids
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
    where tau = TyVarId $ supplyValue ids
collectPat ids ctx (IntPat n)       = (TIntPat n, [], [])
collectPat ids ctx (BoolPat b)      = (TBoolPat b, [], [])
collectPat ids ctx (PVar var)       = (TPVar tau var, [(var, tau)], [])
    where tau = TyVarId $ supplyValue ids
collectPat ids ctx (PApp con pats)  = (TPApp alpha con tpats, binds, (tau :=: tyFun (ts ++ [alpha])):eqs)
    where   (ids', ids'') = split2 ids
            tau = case Map.lookup con (conmap ctx) of
                  Just t -> instantiate ids' t
            (tpats, binds, eqs) = collectPats ids'' ctx pats
            ts = map getTy tpats
            alpha = TyVarId $ supplyValue ids
                
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
