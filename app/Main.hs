module Main (main) where

import Control.Compiler (CompilationResult(..), compile)
import Data.DAG (example1)
import Data.Instr (execute)
import Data.Stack (Stack(Empty))
import System.Exit (ExitCode(..), exitWith)


main :: IO ()
main =
  case compile example1 of
    Left err -> do
      print err
      exitWith (ExitFailure 1)
    Right (CompilationResult _finStack instr) -> do
      execute instr Empty
