module Metafun.Compiler (compile) where

import qualified Language.Kiff.Syntax as Kiff
import qualified Language.CxxMPL.Syntax as MPL

import Metafun.Compiler.State
import Control.Monad
import Data.Maybe

type TProgram = Kiff.Program Kiff.Ty
type TDef = Kiff.Def Kiff.Ty
type TDefEq = Kiff.DefEq Kiff.Ty
type TExpr = Kiff.Expr Kiff.Ty
type TPat = Kiff.Pat Kiff.Ty

compile :: TProgram -> Compiler ()
compile (Kiff.Program typeDecls defs) = do
    mapM_ output platformDefs
    mapM_ compileTypeDecl builtinTys
    mapM_ compileTypeDecl typeDecls
    mapM_ compileDef defs
  where
    platformDefs = [ MPL.Def "Int" [MPL.MetaInt] [MPL.Specialization [MPL.MetaVarDecl "x" MPL.MetaInt] Nothing [] (MPL.TyInt, MPL.VarRef "x")]
                   , MPL.Def "Bool" [MPL.MetaBool] [MPL.Specialization [MPL.MetaVarDecl "b" MPL.MetaBool] Nothing [] (MPL.TyBool, MPL.VarRef "b")]
                   ]
    builtinTys = [Kiff.DataDecl "list" ["a"] [nil, cons]]
      where
        nil = Kiff.DataCon "nil" []
        cons = Kiff.DataCon "cons" [ Kiff.TyVar (Kiff.TvName "a")
                                   , Kiff.TyList $ Kiff.TyVar (Kiff.TvName "a")
                                   ]

compileDef :: TDef -> Compiler ()
compileDef def@(Kiff.Def _ name _ eqs) = do
    name' <- fromMaybe (convertName name) <$> lookupLiftedName name
    let implName = "__impl_" ++ name'
    mtys <- compileDefDecl def
    case eqs of
        [eq] -> do
            (_, spec) <- compileDefEq Nothing eq
            output $ MPL.Def name' mtys [spec]
        _ -> do
            fwdVars <- forM mtys $ \mty -> do
                v <- newVar
                return (v, mty)
            let fwdVarDecls = map (uncurry MPL.MetaVarDecl) fwdVars
                fwdVarRefs = map (MPL.VarRef . fst) fwdVars
                callAlt i = MPL.Typename $ MPL.Call implName (MPL.IntLit i : fwdVarRefs)
                fwdFirst = MPL.Specialization fwdVarDecls Nothing [] (MPL.TyClass, callAlt 0)

            let mkFallthrough altNum (fallthrough, spec) =
                    if fallthrough then [spec, next] else [spec]
                  where
                    next = MPL.Specialization fwdVarDecls (Just $ MPL.IntLit altNum : fwdVarRefs) []
                           (MPL.TyClass, callAlt (altNum + 1))
            output $ MPL.Def name' mtys [fwdFirst]
            specs <- zipWithM (compileDefEq . Just) [0..] eqs
            let specs' = concat $ zipWith mkFallthrough [0..] specs
            output $ MPL.Def implName (MPL.MetaInt:mtys) specs'

compileDefDecl :: TDef -> Compiler [MPL.MetaTy]
compileDefDecl (Kiff.Def t _ _ _) = do
    formalsScope <- getScopeVars
    let mtysScope = map (\ (MPL.MetaVarDecl _ mty) -> mty) formalsScope
    return $ mtysScope ++ mtys
  where
    tys = init $ uncurryTy t
    mtys = map toMetaTy tys

compileDefEq :: Maybe Int -> TDefEq -> Compiler (Bool, MPL.Specialization)
compileDefEq altNum (Kiff.DefEq _ty pats expr) = do
    (varss, spec) <- unzip <$> mapM varsAndSpec pats
    scopeVars <- getScopeVars
    let vars = concat varss
        vars' = scopeVars ++ vars
        fallthrough = not $ all isVar spec
        spec' = map (\ (MPL.MetaVarDecl name _) -> MPL.VarRef name) scopeVars ++ spec
        spec'' = case altNum of
            Just i -> Just $ MPL.IntLit i : spec'
            Nothing -> if fallthrough then Just spec' else Nothing
    body <- withScopeVars vars $ compileExpr expr
    let body' = if null spec' then body else addTypename body
    return (fallthrough, MPL.Specialization vars' spec'' [] (MPL.TyClass, body'))
  where
    varsAndSpec :: TPat -> Compiler ([MPL.MetaVarDecl], MPL.Expr)
    varsAndSpec (Kiff.PVar ty var) =
        return ([MPL.MetaVarDecl var (toMetaTy ty)], MPL.VarRef var)
    varsAndSpec (Kiff.PApp _ con pats) = do
        (varss, specs) <- unzip <$> mapM varsAndSpec pats
        return (concat varss, MPL.Cons con specs)
    varsAndSpec (Kiff.Wildcard ty) = do
        var <- newVar
        varsAndSpec (Kiff.PVar ty var)
    varsAndSpec (Kiff.IntPat _ n) =
        return ([], MPL.Box MPL.TyInt $ MPL.IntLit n)
    varsAndSpec (Kiff.BoolPat _ b) =
        return ([], MPL.Box MPL.TyBool $ MPL.BoolLit b)

    isVar (MPL.VarRef _)  = True
    isVar _               = False

addTypename :: MPL.Expr -> MPL.Expr
addTypename (MPL.Unbox (MPL.Call fun args)) = MPL.Unbox (MPL.Call fun (map addTypename args))
addTypename (MPL.Unbox e)                   = MPL.Unbox $ addTypename e
addTypename (MPL.Box ty e)                  = MPL.Box ty $ addTypename e
addTypename (MPL.PrimBinOp op l r)          = MPL.PrimBinOp op (addTypename l) (addTypename r)
addTypename (MPL.Not e)                     = MPL.Not $ addTypename e
addTypename (MPL.Call fun args)             = MPL.Typename $ MPL.Call fun $ map addTypename args
addTypename (MPL.Cons con args)             = MPL.Cons con $ map addTypename args
addTypename e@(MPL.VarRef _)                = e
addTypename e@(MPL.BoolLit _)               = e
addTypename e@(MPL.IntLit _)                = e
addTypename (MPL.UnaryMinus e)              = MPL.UnaryMinus $ addTypename e

convertTy (Kiff.TyPrimitive Kiff.TyInt)  = MPL.TyInt
convertTy (Kiff.TyPrimitive Kiff.TyBool) = MPL.TyBool

compileTypeDecl :: Kiff.TypeDecl -> Compiler ()
compileTypeDecl (Kiff.DataDecl _name _tvnames cons) = mapM_ compileDataCon cons

compileDataCon :: Kiff.DataCon -> Compiler ()
compileDataCon (Kiff.DataCon name tys) = output decl
  where
    decl = MPL.Def name mtys []
    mtys = map toMetaTy tys

unbox :: MPL.Expr -> MPL.Expr
unbox (MPL.Box _ expr) = expr
unbox expr             = MPL.Unbox expr

uncurryApp :: TExpr -> [TExpr]
uncurryApp (Kiff.App _ f x) = uncurryApp f ++ [x]
uncurryApp expr             = [expr]

compileCall :: MPL.Name -> [MPL.Expr] -> Compiler MPL.Expr
compileCall name args = do
    scopeVars <- getScopeVars
    return $ MPL.Call name (map toArg scopeVars ++ args)
  where
    toArg (MPL.MetaVarDecl name _) = MPL.VarRef name

compileExpr :: TExpr -> Compiler MPL.Expr
compileExpr (Kiff.Var _ var) = do
    lifted <- lookupLiftedName var
    case lifted of
        Nothing -> return $ MPL.VarRef var
        Just name' -> compileCall name' []
compileExpr (Kiff.Con _ con) = return $ MPL.Cons con []
compileExpr e@Kiff.App{} = do
    args' <- mapM compileExpr args
    case fun of
        Kiff.Var _ var -> do
            lifted <- lookupLiftedName var
            case lifted of
                Nothing -> return $ MPL.Call var args'
                Just name' -> compileCall name' args'
        Kiff.Con _ty con -> return $ MPL.Cons con args'
  where
    (fun:args) = uncurryApp e
compileExpr Kiff.Lam{} = error "Lambdas not supported"
compileExpr (Kiff.Let _ defs body) = do
    withLiftedNames names $ do
      mapM_ compileDef defs
      compileExpr body
  where
    names = map (\ (Kiff.Def _ name _ _) -> name) defs
compileExpr (Kiff.PrimBinOp ty op left right) = do
    left' <- unbox <$> compileExpr left
    right' <- unbox <$> compileExpr right
    let op' = compileOp op
    return $ MPL.Box (convertTy ty) $ MPL.PrimBinOp op' left' right'
compileExpr (Kiff.IfThenElse _ cond thn els) =
    MPL.Cond <$> compileExpr cond <*> compileExpr thn <*> compileExpr els
compileExpr (Kiff.IntLit _ n) =
    return $ MPL.Box MPL.TyInt $ MPL.IntLit n
compileExpr (Kiff.BoolLit _ b) =
    return $ MPL.Box MPL.TyBool $ MPL.BoolLit b
compileExpr (Kiff.UnaryMinus _ e) =
    MPL.Box MPL.TyInt . MPL.UnaryMinus . unbox <$> compileExpr e
compileExpr (Kiff.Not _ e) =
    MPL.Box MPL.TyBool . MPL.Not . unbox <$> compileExpr e

toMetaTy :: Kiff.Ty -> MPL.MetaTy
toMetaTy t@(Kiff.TyFun _ _) = MPL.MetaClass $ map toMetaTy $ init $ uncurryTy t
toMetaTy _                  = MPL.MetaClass []

compileOp :: Kiff.PrimitiveOp -> MPL.PrimitiveOp -- TODO: use the same def in both languages
compileOp Kiff.OpAdd = MPL.OpAdd
compileOp Kiff.OpSub = MPL.OpSub
compileOp Kiff.OpMul = MPL.OpMul
compileOp Kiff.OpDiv = MPL.OpDiv
compileOp Kiff.OpMod = MPL.OpMod
compileOp Kiff.OpEq  = MPL.OpEq
compileOp Kiff.OpGt  = MPL.OpGt
compileOp Kiff.OpGe  = MPL.OpGe
compileOp Kiff.OpLe  = MPL.OpLe
compileOp Kiff.OpLt  = MPL.OpLt
compileOp Kiff.OpAnd = MPL.OpAnd
compileOp Kiff.OpOr  = MPL.OpOr

uncurryTy (Kiff.TyFun ty ty')  = ty : uncurryTy ty'
uncurryTy ty                   = [ty]

convertName :: Kiff.VarName -> MPL.Name
convertName = id
