module Language.Kiff.Parser where

import Language.Kiff.Syntax    
    
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language (haskellStyle)
import qualified Text.ParserCombinators.Parsec.IndentParser.Token as IT
import qualified Text.ParserCombinators.Parsec.IndentParser as IP

import Control.Monad
import Data.Char
    
lexer = T.makeTokenParser $ haskellStyle {
          T.caseSensitive = True,
          T.reservedNames = ["if", "then", "else", "True", "False"],
          T.reservedOpNames = ["::", "->", "=", "\\", "*", "/", "+", "-", "%", "&&", "||", "==", "<", ">", "<=", ">="]
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
             
conname = do name@(n:ns) <- identifier
             if isUpper n then return name else fail []

varname = do name@(n:ns) <- identifier
             if isLower n then return name else fail []

boollit = (reserved "True" >> return True) <|>
          (reserved "False" >> return False)
                
ty = do tys <- ty' `sepBy1` (reservedOp "->")
        return $ foldl1 TyFun tys 
    where ty' = buildExpressionParser table term <?> "type expression"
              where  table = [[Infix (whiteSpace >> return TyApp) AssocRight]]
                     term =  parens ty <|> listTy <|> primitiveTy <|> tyVar <|> dataTy

                     listTy = do tyElem <- brackets ty
                                 -- TODO
                                 return $ TyApp (TyData "List") tyElem

                     tyVar = do tv <- varname
                                return $ TyVar tv

                     dataTy = do con <- conname
                                 return $ TyData con
                     
                     primitiveTy = (reserved "Int" >> return (TyPrimitive TyInt)) <|>
                                   (reserved "Bool" >> return (TyPrimitive TyBool))
                                   
expr = buildExpressionParser table term <?> "expression"
    where table = [[Infix (whiteSpace >> return App) AssocLeft],
                   [Infix (reservedOp ":" >> return listcons) AssocRight],
                   [binary "*" OpMul, binary "/" OpDiv],
                   [binary "+" OpAdd, binary "-" OpSub],
                   [binary "||" OpOr],
                   [binary "&&" OpAnd]]
                  -- TODO: ==, <=, ..
              where binary op cb = Infix (reservedOp op >> return (PrimBinOp cb)) AssocLeft
                    listcons left right = App (App (Con "cons") left) right
                  
          term = parens expr <|> list <|> intLit <|> boolLit <|> varref <|> con <|> lam

          list = do elems <- brackets $ expr `sepBy` comma
                    return $ foldr (\x xs -> App (App (Con "cons") x) xs) (Con "nil") elems
                 
          intLit = do n <- natural -- TODO: integer fsck up "-" and "+"
                      return $ IntLit (fromInteger n)
                             
          boolLit = liftM BoolLit boollit

          varref = do var <- varname
                      return $ Var var

          con = do c <- conname
                   return $ Con c

          lam = do reservedOp "\\"
                   pats <- many1 (try pat)
                   reservedOp "->"
                   body <- expr
                   return $ Lam pats body

          ifthenelse = do reserved "if"
                          cond <- expr
                          reserved "then"
                          thn <- expr
                          reserved "else"
                          els <- expr
                          return $ IfThenElse cond thn els

pat = buildExpressionParser table term <?> "pattern"
    where table = [[Infix (reservedOp ":" >> return listcons) AssocRight]]
          term = try (parens conpat) <|> list<|> varpat <|> intpat <|> boolpat <|> parens pat

          conpat = do con <- conname
                      pats <- many pat
                      return $ PApp con pats
                              
          varpat = do v <- varname
                      return $ PVar v

          intpat = do i <- integer
                      return $ IntPat $ fromInteger i

          boolpat = liftM BoolPat boollit

          listcons head rest = PApp "Cons" [head, rest]
                               
          list = do pats <- brackets $ pat `sepBy` comma
                    return $ foldr listcons (PApp "Nil" []) pats
                    

defEq varname = do symbol varname
                   pats <- many pat
                   reservedOp "="
                   body <- expr
                   return $ DefEq pats body

def = do
        msig <- optionMaybe $ IP.lineFold signature
        case msig of
          Just (v, sig)  -> do  eqs <- defEqs v
                                return $ Def v (Just sig) eqs
          Nothing        -> do  v <- lookAhead varname
                                eqs <- defEqs v
                                return $ Def v Nothing eqs

    where signature = do v <- varname
                         reservedOp "::"
                         t <- ty
                         return $ (v, t)

          defEqs v = many1 $ IP.lineFold $ defEq v
                                   
run p input = IP.parse (IP.block p) "" input
    where parseWhole = do whiteSpace
                          x <- p
                          eof
                          return x
                                 
test = unlines ["value :: [Int] -> Int",
                "value [] = 0",
                "value (first:rest) = 10 * (value rest) + first"]
                                 
test' = unlines ["1 + ",
                 "2"]
