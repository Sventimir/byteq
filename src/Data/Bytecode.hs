{-# LANGUAGE GADTs #-}
module Data.Bytecode
  ( bytecode
  , dump
  , writeBytecode
  ) where

import qualified Data.ByteString.Builder as Bytes
import Data.Instr (Instr(..), Seq(..))
import Data.List (intercalate)
import Data.Stack (dumpAddr, encode)


{- Dump a sequence of instructions to a string (useful for debuggind). -}
dump :: Seq s t -> String
dump s = "[" <> intercalate "; " (eachInstr s) <> "]"
  where
  eachInstr :: Seq s t -> [String]
  eachInstr Halt = []
  eachInstr (instr :> s') = dumpInstr instr : eachInstr s'

dumpInstr :: Instr s t -> String
dumpInstr (Push a) = "Push " <> show a
dumpInstr Drop = "Drop"
dumpInstr Dup = "Dup"
dumpInstr Swap = "Swap"
dumpInstr (Dig addr) = "Dig " <> show (dumpAddr addr)
dumpInstr Add = "Add"
dumpInstr Sub = "Sub"
dumpInstr Mul = "Mul"
dumpInstr Div = "Div"
dumpInstr Rem = "Rem"
dumpInstr Eq = "Eq"
dumpInstr Lt = "Lt"
dumpInstr Gt = "Gt"
dumpInstr Not = "Not"
dumpInstr (Cond l r) = "Cond (" <> dump l <> ") (" <> dump r <> ")"
dumpInstr Print = "Print"

{- Convert a sequence of instructions to a bytestring. This creates a raw
   representation of an instruction sequence to be written to a file.
   This essentially renders a 0-terminated string of bytes. Several instructions
   need to store additional data (literals, memory addresses, nested sequences
   of instructions etc.). For this reason we're limited to 255 instruction codes,
   but we're far from reaching this limit in this exercise, so it's fine.
   Also note that a function parsing those bytestrings must be aware of which
   instructions store additional data and how to parse it. -}
bytecode :: Seq s t -> Bytes.Builder
bytecode Halt = Bytes.word8 0
bytecode (instr :> s) = opCode instr <> bytecode s

opCode :: Instr s t -> Bytes.Builder
opCode (Push a) = Bytes.word8 1 <> encode a
opCode Drop = Bytes.word8 2
opCode Dup = Bytes.word8 3
opCode Swap = Bytes.word8 4
opCode (Dig addr) = Bytes.word8 5 <> Bytes.word8 (dumpAddr addr)
opCode Add = Bytes.word8 6
opCode Sub = Bytes.word8 7
opCode Mul = Bytes.word8 8
opCode Div = Bytes.word8 9
opCode Rem = Bytes.word8 10
opCode Eq = Bytes.word8 11
opCode Lt = Bytes.word8 12
opCode Gt = Bytes.word8 13
opCode Not = Bytes.word8 14
-- Note that these sequences need to be parsed until 0 (Halt) is reached.
opCode (Cond l r) = Bytes.word8 15 <> bytecode l <> bytecode r
opCode Print = Bytes.word8 16

writeBytecode :: FilePath -> Seq s t -> IO ()
writeBytecode path = Bytes.writeFile path . bytecode
