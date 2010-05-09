module Metafun where

import Metafun.Compiler
import Metafun.Compiler.State
import Control.Monad.State

import Language.Kiff.Syntax
import Language.Kiff.Parser
import Language.Kiff.Typing
import Language.Kiff.Typing.Infer
import Language.CxxMPL.Unparser

import Data.Supply    
    
mkIds :: IO (Supply TvId)
mkIds = newEnumSupply

compileFile filename = do
  parseRes <- parseFile filename
  case parseRes of
    Left errors -> do print errors
                      return Nothing
    Right prog@(Program decls defs) -> do
                            ids <- mkIds
                            let (ids', ids'') = split2 ids
                                ctx = mkCtx ids'
                            case inferDefs ids'' ctx defs of
                              Left errors -> do putStrLn $ unlines $ map show errors
                                                return Nothing
                              Right (ctx', tdefs) -> do let tprog = Program decls tdefs
                                                            mpl = evalState (compile tprog) mkCompilerState
                                                        return $ Just mpl

test = do compileRes <- compileFile filename
          case compileRes of
            Nothing -> return ()
            Just mpl -> print $ unparse mpl
    where filename = "lorentey-c++-tmp.hs"
