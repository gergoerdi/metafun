module Language.Kiff.CallGraph (sortDefs) where

import Language.Kiff.Syntax

import qualified Data.Set as S
import qualified Data.Graph as G

sortDefs :: [Def] -> [[Def]]
sortDefs defs = map fromSCC (G.stronglyConnComp nodes)
    where  names = newBinds defs
           nodes = map edgesFromDef defs
           fromSCC (G.AcyclicSCC v)  = [v]
           fromSCC (G.CyclicSCC vs)  = vs

           edgesFromDef :: Def -> (Def, VarName, [VarName])
           edgesFromDef def@(Def name _ _) = (def, name, S.toList $ collectDef names def)

newBinds :: [Def] -> S.Set VarName
newBinds defs = S.fromList $ map (\ (Def name _ _) -> name) defs
            
collectDef :: S.Set VarName -> Def -> S.Set VarName
collectDef names (Def _ _ defeqs) = S.unions $ map (collectDefEq names) defeqs

collectDefEq :: S.Set VarName -> DefEq -> S.Set VarName
collectDefEq names (DefEq pats expr) = collectExpr names' expr
    where names' = names `S.difference` (collectPats pats)
              
collectPat :: Pat -> S.Set VarName
collectPat (PVar v)       = S.singleton v
collectPat (PApp _ pats)  = S.unions $ map collectPat pats
collectPat _              = S.empty

collectPats :: [Pat] -> S.Set VarName
collectPats = S.unions . map collectPat

collectExpr :: S.Set VarName -> Expr -> S.Set VarName
collectExpr names (Var v) | v `S.member` names = S.singleton v
collectExpr names (App f x) = S.unions $ map (collectExpr names) [f, x]
collectExpr names (PrimBinOp _ l r) = S.unions $ map (collectExpr names) [l, r]
collectExpr names (IfThenElse cond thn els) = S.unions $ map (collectExpr names) [cond, thn, els]
collectExpr names (Lam pats body) = collectExpr names' body
    where names' = names `S.difference` (collectPats pats)
collectExpr names (Let defs body) = S.unions $ (collectExpr names' body):(map (collectDef names) defs)
    where names' = names `S.difference` vars
          vars = newBinds defs
collectExpr names _ = S.empty
