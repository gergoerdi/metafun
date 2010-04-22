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
          P.reservedNames = ["if", "then", "else", "where"],
          P.reservedOpNames = ["::", "->", "="]
         }
    
brackets   = P.brackets lexer
colon      = P.colon lexer              
comma      = P.comma lexer
semi       = P.semi lexer
parens     = P.parens lexer
integer    = P.integer lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
identifier = P.identifier lexer
whiteSpace = P.whiteSpace lexer
             
ty :: Parser Ty
ty = do tys <- ty' `sepBy1` (reservedOp "->")        
        return $ foldl1 TyFun tys 
    where ty' = buildExpressionParser table term <?> "type expression"
              where  table = [[Infix (whiteSpace >> return TyApp) AssocRight]]
                     term =  parens ty <|> listTy <|> primitiveTy <|> simpleTy

                     listTy = do tyElem <- brackets ty
                                 -- TODO
                                 return $ TyApp (TyData "List") tyElem
                            
                     simpleTy = do tyname@(n:ns) <- identifier
                                   -- TODO: TyVar or TyData?
                                   return $ (if (isUpper n) then TyData else TyVar) tyname

                     primitiveTy = (reserved "Int" >> return (TyPrimitive TyInt)) <|>
                                   (reserved "Bool" >> return (TyPrimitive TyBool))

run :: Parser a -> String -> Either ParseError a
run p input = parse parseWhole "" input
    where parseWhole = do whiteSpace
                          x <- p
                          eof
                          return x
