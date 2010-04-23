module Language.Kiff.LineParser (runLineParser) where

import Text.ParserCombinators.Parsec
    
type SimpleLine = (SourcePos, String)
data NestedLine = NestedLine SimpleLine [NestedLine] deriving Show

type IndentSize = Int
type LineParser a = GenParser Char IndentSize a

skipIndent :: LineParser ()
skipIndent = do indent <- getState
                count indent space
                return ()

withNewIndent p = do
  i <- lookAhead $ do
        skipIndent
        spaces <- many1 space
        return $ length spaces
  s <- getState
  updateState (+i)
  r <- p
  setState s
  return r
                           
block :: LineParser NestedLine
block = do
  skipIndent
  notFollowedBy space
  pos <- getPosition           
  head <- manyTill anyChar newline
  rest <- option [] $ try $ withNewIndent $ many1 (try block)
  return $ NestedLine (pos, head) rest

blocks = do r <- many block
            eof
            return r

runLineParser = runParser blocks 0                   
                   
run :: String -> Either ParseError [NestedLine]
run input = runLineParser "" input
                  
test = unlines ["foo",
                "  foo1",
                "  foo2",
                "    foo21",
                "  foo3",
                "bar"]

test' = unlines ["foo",
                 " bar",
                 "  baz",
                 " quux"]
