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

test = do
  Right prog@(Program decls defs) <- parseFile filename
  ids <- mkIds
  let (ids', ids'') = split2 ids
      ctx = mkCtx ids'
      Right (ctx', tdefs) = inferDefs ids'' ctx defs
      tprog = Program decls tdefs
  print ctx'
  print tprog
  let mpl = evalState (compile tprog) mkCompilerState
  print $ unparse mpl
      where filename = "lorentey-c++-tmp.hs"
      --where filename = "sum.hs"
