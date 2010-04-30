module Language.Kiff.CallGraph (sortDefs) where

import Language.Kiff.Syntax

import qualified Data.Set as S
import qualified Data.Graph as G

sortDefs :: [Def a] -> [[Def a]]
sortDefs defs = map fromSCC (G.stronglyConnComp nodes)
    where  names = newBinds defs
           nodes = map edgesFromDef defs
           fromSCC (G.AcyclicSCC v)  = [v]
           fromSCC (G.CyclicSCC vs)  = vs

           edgesFromDef :: Def a -> (Def a, VarName, [VarName])
           edgesFromDef def@(Def _ name _ _) = (def, name, S.toList $ collectDef names def)

newBinds :: [Def a] -> S.Set VarName
newBinds defs = S.fromList $ map (\ (Def _ name _ _) -> name) defs
            
collectDef :: S.Set VarName -> Def a -> S.Set VarName
collectDef names (Def _ _ _ defeqs) = S.unions $ map (collectDefEq names) defeqs

collectDefEq :: S.Set VarName -> DefEq a -> S.Set VarName
collectDefEq names (DefEq _ pats expr) = collectExpr names' expr
    where names' = names `S.difference` (collectPats pats)
              
collectPat :: Pat a -> S.Set VarName
collectPat (PVar _ v)       = S.singleton v
collectPat (PApp _ _ pats)  = S.unions $ map collectPat pats
collectPat _                = S.empty

collectPats :: [Pat a] -> S.Set VarName
collectPats = S.unions . map collectPat

collectExpr :: S.Set VarName -> Expr a -> S.Set VarName
collectExpr names (Var _ v) | v `S.member` names  = S.singleton v
collectExpr names (App _ f x)                     = S.unions $ map (collectExpr names) [f, x]
collectExpr names (PrimBinOp _ _ l r)             = S.unions $ map (collectExpr names) [l, r]
collectExpr names (IfThenElse _ cond thn els)     = S.unions $ map (collectExpr names) [cond, thn, els]
collectExpr names (Lam _ pats body)               = collectExpr names' body
    where names' = names `S.difference` (collectPats pats)
collectExpr names (Let _ defs body)               = S.unions $ (collectExpr names' body):(map (collectDef names) defs)
    where names' = names `S.difference` vars
          vars = newBinds defs
collectExpr names _                               = S.empty
