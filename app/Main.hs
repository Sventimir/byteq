module Main (main) where

import Control.Compiler (CompilationResult(..), compile)
import qualified Data.DAG as DAG
import Data.Instr (execute, dump, writeBytecode)
import Data.Stack (Stack(Empty))
import System.Exit (ExitCode(..), exitWith)
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  case selectCmd (head args) of
    Just c -> c (tail args)
    Nothing -> error "Unknown command"

selectCmd :: String -> Maybe ([String] -> IO ())
selectCmd "build" = Just build
selectCmd "interp" = Just interp
selectCmd _ = Nothing

interp :: [String] -> IO ()
interp args = do
  let program = case selectProgram (head args) of
                  Just p -> p
                  Nothing -> error "Unknown program"
  case compile program of
    Left err -> do
      print err
      exitWith (ExitFailure 1)
    Right (CompilationResult _finStack instr) -> do
      putStrLn $ dump instr
      execute instr Empty

build :: [String] -> IO ()
build args = do
  let program = case selectProgram (head args) of
                  Just p -> p
                  Nothing -> error "Unknown program"
  case compile program of
    Left err -> do
      print err
      exitWith (ExitFailure 1)
    Right (CompilationResult _finStack instr) -> do
      putStrLn $ dump instr
      writeBytecode (head args <> ".bc") instr

selectProgram :: String -> Maybe (DAG.DAG ())
selectProgram "example1" = Just DAG.example1
selectProgram "fib" = Just DAG.exampleFib
selectProgram _ = Nothing
