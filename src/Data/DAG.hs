{-# LANGUAGE DataKinds, GADTs, TypeOperators #-}
module Data.DAG
  ( DAG(..)
  , Variable(..)
  , example1
  ) where

import Data.Instr (Instr(..))
import Data.Type (TType(..))


{- This data structure stores data about a variable in program's memory:
   * name
   * type -}
data Variable a = Variable (TType a) String

{- For Direct Acyclic Graph of program's execution. Each DAG
   constitutes an expression and is parametrised by the type of that
   expression.
   NOTE that PrintVal cannot be represented as UnOp, because it
   doesn't produce any result and therefore the stack shrinks as a
   result of its execution. UnOp on the other hand expects an
   instruction, which produces a result, thus preserving the stack
   size. -}
data DAG a where
  Literal :: TType a -> a -> DAG a
  Assignment :: Variable a -> DAG a -> DAG ()
  Var :: Variable a -> DAG a
  UnOp :: Instr (a ': r) (b ': r) -> DAG a -> DAG a
  BinOp :: Instr (a ': b ': r) (c ': r) -> DAG a -> DAG b -> DAG c
  If :: DAG Bool -> DAG a -> DAG a -> DAG a
  PrintVal :: DAG a -> DAG ()
  Seq :: DAG () -> DAG b -> DAG b

{- This is an example of a DAG representing the following program:
   a = 5
   b = 4 + a
   print a
   print b -}
example1 :: DAG ()
example1 = (Assignment (Variable TInt "a") (Literal TInt 5))
  `Seq` (Assignment (Variable TInt "b")
        (BinOp Add (Literal TInt 4) (Var (Variable TInt "a"))))
  `Seq` (PrintVal (Var (Variable TInt "a")))
  `Seq` (PrintVal (Var (Variable TInt "b")))
