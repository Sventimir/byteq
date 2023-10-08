module Main (main) where

import Control.Compiler (compile)
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
    Right instr -> do
      exitCode <- execute instr Empty
      case exitCode of
        0 -> exitWith ExitSuccess
        _ -> exitWith (ExitFailure exitCode)
