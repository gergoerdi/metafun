module Language.Kiff.Parser (parseFile) where

import Language.Kiff.Syntax    
    
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as T
import qualified Text.ParserCombinators.Parsec.Language as L
import qualified Text.ParserCombinators.Parsec.IndentParser.Token as IT
import qualified Text.ParserCombinators.Parsec.IndentParser as IP

import Control.Monad
import Data.Char
import Data.Either

import Control.Applicative ((<$>)) -- IndentParser uses Parsec 2, which doesn't expose an Applicative instance...

lexer = T.makeTokenParser $ L.haskellStyle {
          T.reservedNames = ["if", "then", "else", "True", "False", "let", "in", "data", "type", "lambda"],
          T.reservedOpNames = ["::", "->", "=", "\\", "_", "|",
                               "*", "/", "+", "-", "%",
                               "!", "&&", "||",
                               "==", "<", ">", "<=", ">="]
         }
    
brackets   = IT.brackets lexer
colon      = IT.colon lexer              
comma      = IT.comma lexer
semi       = IT.semi lexer
parens     = IT.parens lexer
integer    = IT.integer lexer
natural    = IT.natural lexer
reserved   = IT.reserved lexer
reservedOp = IT.reservedOp lexer
identifier = IT.identifier lexer
whiteSpace = IT.whiteSpace lexer
symbol     = IT.symbol lexer
             
conname = do name@(n:_) <- identifier
             if isUpper n then return name else fail []

varname = do name@(n:_) <- identifier
             if isLower n then return name else fail []

boollit = (reserved "True" >> return True) <|>
          (reserved "False" >> return False)
                
ty = do tys <- ty' `sepBy1` (reservedOp "->")
        return $ foldr1 TyFun tys 
    where ty' = buildExpressionParser table term <?> "type expression"
              where  table = [[Infix (whiteSpace >> return TyApp) AssocRight]]
                     term =  parens ty <|> listTy <|> try primitiveTy <|> try tyVar <|> dataTy

                     listTy = TyList <$> brackets ty
                     tyVar = TyVar <$> TvName <$> varname
                     dataTy = TyData <$> conname                     
                     primitiveTy = (reserved "Int" >> return (TyPrimitive TyInt)) <|>
                                   (reserved "Bool" >> return (TyPrimitive TyBool))
                                   
expr = buildExpressionParser table term <?> "expression"
    where table = [[Infix (whiteSpace >> return (App ())) AssocLeft],
                   [Infix (reservedOp ":" >> return listcons) AssocRight],
                   [binary "*" OpMul, binary "/" OpDiv],
                   [binary "+" OpAdd, binary "-" OpSub, Prefix (reservedOp "-" >> return (UnaryMinus ()))],
                   [binary "%" OpMod],
                   [binary "==" OpEq, binary "<=" OpLe, binary "<" OpLt, binary ">" OpGt, binary ">=" OpGe],
                   [Prefix (reservedOp "!" >> return (Not ()))],
                   [binary "||" OpOr],
                   [binary "&&" OpAnd]]
                  -- TODO: ==, <=, ..
              where binary op cb = Infix (reservedOp op >> return (PrimBinOp () cb)) AssocLeft
                    listcons left right = App () (App () (Con () "cons") left) right
                  
          term = parens expr <|> list <|> intLit <|> boolLit <|> varref <|> con <|> lam <|> vars

          vars = do reserved "let"
                    defs <- IP.block $ many1 $ def
                    reserved "in"
                    body <- expr
                    return $ Let () defs body
                    
                 
          list = do elems <- brackets $ expr `sepBy` comma
                    return $ foldr (\x xs -> App () (App () (Con () "cons") x) xs) (Con () "nil") elems
                 
          intLit = IntLit () <$> fromInteger <$> natural -- TODO: integerl fscks up "-" and "+"                             
          boolLit = BoolLit () <$> boollit
          varref = Var () <$> varname
          con = Con () <$> conname

          lam = do reservedOp "\\" <|> reserved "lambda"
                   pats <- many1 (try pat)
                   reservedOp "->"
                   body <- expr
                   return $ Lam () pats body

          ifthenelse = do reserved "if"
                          cond <- expr
                          reserved "then"
                          thn <- expr
                          reserved "else"
                          els <- expr
                          return $ IfThenElse () cond thn els

pat = buildExpressionParser table term <?> "pattern"
    where table = [[Infix (reservedOp ":" >> return listcons) AssocRight]]
          term = try (parens conpat) <|> list <|> wildcard <|> varpat <|> intpat <|> boolpat <|> parens pat

          conpat = do con <- conname
                      pats <- many pat
                      return $ PApp () con pats
                              
          varpat = PVar () <$> varname

          wildcard = do reservedOp "_"
                        return $ Wildcard ()
                             
          intpat = IntPat () <$> fromInteger <$> integer

          boolpat = BoolPat () <$> boollit

          listcons head rest = PApp () "cons" [head, rest]
                               
          list = do pats <- brackets $ pat `sepBy` comma
                    return $ foldr listcons (PApp () "nil" []) pats
                    

defEq varname = do _ <- try $ symbol varname
                   pats <- many pat
                   reservedOp "="
                   body <- expr
                   return $ DefEq () pats body

def = do
        msig <- optionMaybe $ try $ IP.lineFold signature
        case msig of
          Just (v, sig)  -> do  eqs <- defEqs v
                                return $ Def () v (Just sig) eqs
          Nothing        -> do  v <- lookAhead varname
                                eqs <- defEqs v
                                return $ Def () v Nothing eqs

    where signature = do v <- varname
                         reservedOp "::"
                         t <- ty
                         return $ (v, t)

          defEqs v = many1 $ IP.lineFold $ defEq v                     

typedecl = datadecl <|> typealias <?> "Type declaration"
    where datadecl = do reserved "data"
                        n <- conname
                        tvs <- many varname
                        reservedOp "="
                        cons <- IP.lineFold $ datacon `sepBy` (reservedOp "|")
                        return $ DataDecl n tvs cons
                               
          typealias = do reserved "type"
                         n <- conname
                         tvs <- many varname
                         reservedOp "="
                         ty <- IP.lineFold $ ty
                         return $ TypeAlias n tvs ty
                                
          datacon = do n <- conname
                       tys <- many ty
                       return $ DataCon n tys

program = do (decls, defs) <- liftM partitionEithers $ many $ whiteSpace >> (decl <|> vardef)
             return $ Program decls defs
                  
    where decl = Left <$> typedecl                           
          vardef = Right <$> def

parseFile = IP.parseFromFile parseWhole
    where parseWhole = do prog <- program
                          eof
                          return prog
