module Language.Kiff.Parser where

import Language.Kiff.Syntax    

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellStyle)

import Control.Monad
import Data.Char
    
lexer :: P.TokenParser ()
lexer = P.makeTokenParser $ haskellStyle {
          P.caseSensitive = True,
          P.reservedNames = ["if", "then", "else", "where", "True", "False"],
          P.reservedOpNames = ["::", "->", "=", "\\", "*", "/", "+", "-", "%", "&&", "||", "==", "<", ">", "<=", ">="]
         }
    
brackets   = P.brackets lexer
colon      = P.colon lexer              
comma      = P.comma lexer
semi       = P.semi lexer
parens     = P.parens lexer
integer    = P.integer lexer
natural    = P.natural lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
identifier = P.identifier lexer
whiteSpace = P.whiteSpace lexer

conname :: Parser String
conname = do name@(n:ns) <- identifier
             if isUpper n then return name else fail []

varname :: Parser String
varname = do name@(n:ns) <- identifier
             if isLower n then return name else fail []

boollit :: Parser Bool
boollit = (reserved "True" >> return True) <|>
          (reserved "False" >> return False)
                
ty :: Parser Ty
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
                                   
expr :: Parser Expr
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

pat = try (parens conpat) <|> varpat <|> intpat <|> boolpat <|> parens pat
      where conpat = do con <- conname
                        pats <- many pat
                        return $ PApp con pats
                              
            varpat = do v <- varname
                        return $ PVar v

            intpat = do i <- integer
                        return $ IntPat $ fromInteger i

            boolpat = liftM BoolPat boollit

            -- TODO: list patterns
                                             
                                   
run :: Parser a -> String -> Either ParseError a
run p input = parse parseWhole "" input
    where parseWhole = do whiteSpace
                          x <- p
                          eof
                          return x
