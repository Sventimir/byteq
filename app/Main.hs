{-# LANGUAGE DataKinds #-}
module Main (main) where

import Control.Compiler (CompilationResult(..), compile)
import Control.Monad (when)
import Data.Bytecode (BytecodeParseState(..), dump, writeBytecode, parseBytecode)
import qualified Data.ByteString as Bytes
import qualified Data.DAG as DAG
import Data.Instr (execute)
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
selectCmd "dump" = Just showInstr
selectCmd "run" = Just run
selectCmd "interp" = Just interp
selectCmd _ = Nothing

interp :: [String] -> IO ()
interp args = do
  let program = case selectProgram (head args) of
                  Just p -> p
                  Nothing -> error "Unknown program"
  CompilationResult _finStack instr <- compileDAG program
  execute instr Empty

run :: [String] -> IO ()
run args = do
  BytecodeParseState _bytes _finStack instr <- loadBytecode (head args)
  execute instr Empty

build :: [String] -> IO ()
build args = do
  let program = case selectProgram (head args) of
                  Just p -> p
                  Nothing -> error "Unknown program"
  CompilationResult _finStack instr <- compileDAG program
  writeBytecode (head args <> ".bc") instr

showInstr :: [String] -> IO ()
showInstr args = do
  case selectProgram (head args) of
    Just dag -> do
      CompilationResult _finstack instr <- compileDAG dag
      putStrLn $ dump instr
    Nothing -> do
      BytecodeParseState _ _ instr <- loadBytecode (head args)
      putStrLn $ dump instr
      

selectProgram :: String -> Maybe (DAG.DAG ())
selectProgram "example1" = Just DAG.example1
selectProgram "example2" = Just DAG.example2
selectProgram "fib" = Just DAG.exampleFib
selectProgram _ = Nothing

loadBytecode :: String -> IO (BytecodeParseState '[])
loadBytecode path = do
  bytes <- Bytes.readFile path
  case parseBytecode bytes of
    Left err -> do
      print err
      exitWith (ExitFailure 1)
    Right r@(BytecodeParseState bs _finStack _instr) -> do
      when (Bytes.length bs > 0) $ do
        putStr "Trailing bytes in bytecode: "
        print bs
      return r

compileDAG :: DAG.DAG () -> IO (CompilationResult '[])
compileDAG dag = do
  case compile dag of
    Left err -> do
      print err
      exitWith (ExitFailure 1)
    Right r@(CompilationResult _finStack _instr) -> do
      return r 
