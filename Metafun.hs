module Main where

import Metafun.Compiler
import Metafun.Compiler.State (runCompiler)
    
import qualified Language.Kiff.Syntax as Kiff
import qualified Language.CxxMPL.Syntax as MPL

import Language.Kiff.Parser
import Language.Kiff.Typing
import Language.Kiff.Typing.Infer
import Language.Kiff.Typing.Errors
import Language.CxxMPL.Unparser

import Control.Monad.State (evalState)
import System.Path (splitExt)
import Text.PrettyPrint (render)
import System.Environment (getArgs, getProgName)
    
compileFile filename = do
  parseRes <- parseFile filename
  case parseRes of
    Left errors -> do print errors
                      return Nothing
    Right prog@(Kiff.Program decls defs) -> case infer prog of
                                              Left errors -> do putStrLn $ render $ output errors
                                                                return Nothing
                                              Right tprog -> do let mpl = runCompiler (compile tprog)
                                                                return $ Just mpl

main' filename = do putStrLn $ unwords ["Compiling", filename]
                    compileRes <- compileFile filename
                    case compileRes of
                      Nothing -> return ()
                      Just mpl -> do putStrLn $ unwords ["Creating", filename']
                                     writeFile filename' $ render $ unparse mpl
                                     -- putStrLn $ render $ unparse mpl
    where (basename, ext) = splitExt filename
          filename' = (if ext == ".kiff" then basename else filename) ++ ".cc"
                                                                       
test = main' "test/lorentey-c++-tmp.kiff"

main = do args <- getArgs          
          case args of
            [filename] -> main' filename
            _ -> do progname <- getProgName
                    error $ unwords ["Usage:", progname, "inputfile.kiff"]
