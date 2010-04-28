module Metafun where

import Metafun.Compiler
import Language.Kiff.Syntax
import Language.Kiff.Parser
import Language.Kiff.Typing.Infer
import Data.Supply    
import Language.CxxMPL.Unparser
    
mkIds :: IO (Supply TvId)
mkIds = newEnumSupply

test = do Right prog@(Program decls defs) <- parseFile "lorentey-c++-tmp.hs"
          ids <- mkIds
          let (ids', ids'') = split2 ids
              ctx = mkCtx ids'
              Right (ctx', tdefs) = inferDefs ids'' ctx defs
              tprog = TProgram decls tdefs
          print ctx'
          let mpl = compile tprog
          print $ unparse mpl
