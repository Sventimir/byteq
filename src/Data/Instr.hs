{-# LANGUAGE DataKinds, GADTs, KindSignatures, RankNTypes, TypeOperators #-}
module Data.Instr
  ( Instr(..)
  , Seq(..)
  , execute
  , exec
  , append
  , dump
  , bytecode
  , writeBytecode
  ) where

import Data.Int (Int64)
import qualified Data.ByteString.Builder as Bytes
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Stack (Stack(..), StackItem(..), OnStack, push, dupDig, dumpAddr)


{- This type represents VM instructions. It is parameterised by the
   shape of the stack before and after executing the instruction.
   Because instructions only interact with the stack and the shape
   of the stack is known prior to execution, the execution itself is
   safe. It cannot fail. We are certain at all times that:
   * if we access a value on the stack, it is there (stack is not empty)
   * the value we access has exactly the type we expect. -}
data Instr :: [Type] -> [Type] -> Type where
  -- stack manipulation
  Push :: StackItem a => a -> Instr r (a ': r)
  Drop :: Instr (a ': r) r
  Dup :: StackItem a => Instr (a ': r) (a ': a ': r)
  Swap :: (StackItem a, StackItem b) => Instr (a ': b ': r) (b ': a ': r)
  Dig :: StackItem a => OnStack a s -> Instr s (a ': s)
  -- Arithmetic
  Add :: Instr (Int64 ': Int64 ': r) (Int64 ': r)
  Sub :: Instr (Int64 ': Int64 ': r) (Int64 ': r)
  Mul :: Instr (Int64 ': Int64 ': r) (Int64 ': r)
  Div :: Instr (Int64 ': Int64 ': r) (Int64 ': r)
  Rem :: Instr (Int64 ': Int64 ': r) (Int64 ': r)
  -- Comparison
  Eq :: StackItem a => Instr (a ': a ': r) (Bool ': r)
  Lt :: Instr (Int64 ': Int64 ': r) (Bool ': r)
  Gt :: Instr (Int64 ': Int64 ': r) (Bool ': r)
  -- Logic
  Not :: Instr (Bool ': r) (Bool ': r)
  Cond :: Seq r s -> Seq r s -> Instr (Bool ': r) s
  -- IO
  Print :: StackItem a => Instr (a ': r) r


data Seq :: [Type] -> [Type] -> Type where
  Halt :: Seq s s
  (:>) :: Instr r s -> Seq s t -> Seq r t

infixr 5 :>

{- Execute a sequence of instructions. Execution always begins
   with an empty Stack and exits with an arbitrary stack. It
   is possible to refine the type to enforce returning an empty
   stack in order to force the compiler to introduce some
   garbage collection strategy. This is not done yet, though.
   This function (somewhat sadly) returns IO, because we want to be
   able to print in the middle of computation. -}
execute :: Seq s t -> Stack s -> IO ()
execute instr stack = do
  _ <- exec instr stack
  return ()
  
exec :: Seq s s' -> Stack s -> IO (Stack s')
exec Halt stack = return stack
exec (instr :> s) stack = execInstr instr stack >>= exec s


execInstr :: Instr r s -> Stack r -> IO (Stack s)
execInstr (Push a) stack = return (a `push` stack)
execInstr Drop (Item _ stack) = return stack
execInstr Dup stack@(Item a _) =
  return (a `push` stack)
execInstr Swap (Item a (Item b stack)) =
  return (push b $ push a stack)
execInstr (Dig witness) stack =
  return $ dupDig witness stack
execInstr Add (Item a (Item b stack)) =
  return ((a + b) `push` stack)
execInstr Sub (Item a (Item b stack)) =
  return ((a - b) `push` stack)
execInstr Mul (Item a (Item b stack)) =
  return ((a * b) `push` stack)
execInstr Div (Item a (Item b stack)) =
  return ((a `div` b) `push` stack)
execInstr Rem (Item a (Item b stack)) =
  return ((a `rem` b) `push` stack)
execInstr Eq (Item a (Item b stack)) =
  return ((a == b) `push` stack)
execInstr Lt (Item a (Item b stack)) =
  return ((a < b) `push` stack)
execInstr Gt (Item a (Item b stack)) =
  return ((a > b) `push` stack)
execInstr Not (Item a stack) =
  return (not a `push` stack)
execInstr (Cond l r) (Item a stack) =
  exec (if a then l else r) stack
execInstr Print (Item a stack) = do
  print a
  return stack

{- Add an instruction to the end of a sequence. Unfortunately, this
   operation is O(n) in the length of the sequence we append to.
   This could perhaps be optimised with a smarter representation of
   the Seq type. -}
append :: Seq s t -> Instr t u -> Seq s u
append Halt i = i :> Halt
append (l :> s) r = l :> append s r

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
