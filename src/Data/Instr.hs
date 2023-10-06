{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators #-}
module Data.Instr
  ( Instr(..)
  , Seq(..)
  , execute
  , exec
  ) where

import Data.Kind (Type)
import Data.Stack (Stack(..), StackItem, OnStack, push, dig)


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
  Dig :: StackItem a => OnStack a s r -> Instr r (a ': s)
  -- Arithmetic
  Add :: Instr (Int ': Int ': r) (Int ': r)
  Sub :: Instr (Int ': Int ': r) (Int ': r)
  Mul :: Instr (Int ': Int ': r) (Int ': r)
  Div :: Instr (Int ': Int ': r) (Int ': r)
  Rem :: Instr (Int ': Int ': r) (Int ': r)
  -- Comparison
  Eq :: Instr (a ': a ': r) (Bool ': r)
  Lt :: Instr (Int ': Int ': r) (Bool ': r)
  Gt :: Instr (Int ': Int ': r) (Bool ': r)
  -- Logic
  Not :: Instr (Bool ': r) (Bool ': r)
  If :: Seq r s -> Seq r s -> Instr (Bool ': r) s
  -- IO
  Print :: Instr (a ': r) r


data Seq :: [Type] -> [Type] -> Type where
  Halt :: Seq s s
  (:>) :: Instr r s -> Seq s t -> Seq r t

infixr 5 :>

{- Execute a sequence of instructions. Execution always begins
   with an empty Stack and finishes when the stack holds just
   one integer. The integer becomes the exit code of the program.
   The requirement that there must be nothing more on the stack
   forces the compiler to implement some garbage collection
   strategy so that unused values are removed from the stack.
   The type signature does not ensure they will be removed as soon
   as they're not needed, but they must be remove eventually (before
   the program terminates). In practice, though, it will be easiest
   to remove them immediately.
   This function (somewhat sadly) returns IO, because we want to be
   able to print in the middle of computation. -}
execute :: Seq s '[Int] -> Stack s -> IO Int
execute instr stack = do
  Item exitCode Empty <- exec instr stack
  return exitCode
  
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
  let (a, stack') = dig witness stack in
  return (a `push` stack')
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
execInstr (If l r) (Item a stack) =
  exec (if a then l else r) stack
execInstr Print (Item a stack) = do
  print a
  return stack
