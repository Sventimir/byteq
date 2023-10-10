{-# LANGUAGE DataKinds, GADTs, KindSignatures, RankNTypes, TypeOperators #-}
module Data.Instr
  ( Instr(..)
  , Seq(..)
  , execute
  , exec
  , append
  ) where

import Data.Functor.Identity (Identity(..))
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Stack (Stack(..), StackItem(..), OnStack, push, dupDig)
import Data.Type (TType(..))


type ExecStack = Stack Identity

{- This type represents VM instructions. It is parameterised by the
   shape of the stack before and after executing the instruction.
   Because instructions only interact with the stack and the shape
   of the stack is known prior to execution, the execution itself is
   safe. It cannot fail. We are certain at all times that:
   * if we access a value on the stack, it is there (stack is not empty)
   * the value we access has exactly the type we expect. -}
data Instr :: [Type] -> [Type] -> Type where
  -- stack manipulation
  Push :: StackItem a => TType a -> a -> Instr r (a ': r)
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
execute :: Seq s t -> ExecStack s -> IO ()
execute instr stack = do
  _ <- exec instr stack
  return ()
  
exec :: Seq s s' -> ExecStack s -> IO (ExecStack s')
exec Halt stack = return stack
exec (instr :> s) stack = execInstr instr stack >>= exec s


execInstr :: Instr r s -> ExecStack r -> IO (ExecStack s)
execInstr (Push _ a) stack = return (Identity a `push` stack)
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
  return (Identity (a == b) `push` stack)
execInstr Lt (Item a (Item b stack)) =
  return (Identity (a < b) `push` stack)
execInstr Gt (Item a (Item b stack)) =
  return (Identity (a > b) `push` stack)
execInstr Not (Item a stack) =
  return ((not <$> a) `push` stack)
execInstr (Cond l r) (Item a stack) =
  exec (if runIdentity a then l else r) stack
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
