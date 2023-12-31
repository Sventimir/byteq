{-# LANGUAGE DataKinds, GADTs, RankNTypes, TypeOperators #-}
module Data.DAG
  ( DAG(..)
  , Variable(..)
  , example1
  , example2
  , exampleFib
  ) where

import Data.Instr (Instr(..))
import Data.Type (TType(..))
import Data.Type.Equality ((:~:)(Refl), TestEquality(..))
import Data.Stack (StackItem)


{- This data structure stores represents a variable in program's memory:
   * name
   * type -}
data Variable a = Variable (TType a) String

{- TestEquality is class similar to Eq, but for parametric types. Not
   only does it check if given values are equal, but also provides a
   proof of type equality for their type parameters (called Refl). In
   the presence of this proof Haskell compiler can convince itself
   that these type parameters are equal and use this information for
   further type checking. -}
instance TestEquality Variable where
  testEquality (Variable t n) (Variable t' n') = do
    Refl <- testEquality t t'
    if n == n' then Just Refl else Nothing

instance Show (Variable a) where
  show (Variable t "") = show t
  show (Variable t n) = n <> " : " <> show t

{- For Direct Acyclic Graph of program's execution. Each DAG
   constitutes an expression and is parametrised by the type of that
   expression.
   NOTE that PrintVal cannot be represented as UnOp, because it
   doesn't produce any result and therefore the stack shrinks as a
   result of its execution. UnOp on the other hand expects an
   instruction, which produces a result, thus preserving the stack
   size. -}
data DAG a where
  Literal :: StackItem a => TType a -> a -> DAG a
  Assignment :: StackItem a => Variable a -> DAG a -> DAG ()
  Var :: StackItem a => Variable a -> DAG a
  Pass :: DAG ()
  UnOp :: (StackItem a, StackItem b) =>
          TType a -> TType b -> (forall r. Instr (a ': r) (b ': r)) ->
          DAG a -> DAG b
  BinOp :: (StackItem a, StackItem b, StackItem c) =>
           TType a -> TType b -> TType c ->
           (forall r. Instr (a ': b ': r) (c ': r)) ->
           DAG a -> DAG b -> DAG c
  If :: StackItem a => DAG Bool -> DAG a -> DAG a -> DAG a
  PrintVal :: StackItem a => DAG a -> DAG ()
  Seq :: StackItem a => DAG () -> DAG a -> DAG a

instance Show (DAG a) where
  show (Literal _ a) = show a
  show (Assignment v expr) = show v <> " = " <> show expr
  show (Var v) = "(" <> show v <> ")"
  show Pass = "()"
  show (UnOp tArg tRet _ expr) =
    "(op : " <> show tArg <> " ->" <> show tRet <> ") " <>
    "(" <> show expr <> ")"
  show (BinOp _ _ _ _ left right) =
    "(" <> show left <> ") * (" <> show right <> ")"
  show (If condExpr thenExpr elseExpr) =
    "if " <> show condExpr <> " then " <> show thenExpr <>
    " else " <> show elseExpr
  show (PrintVal expr) = "print (" <> show expr <> ")"
  show (Seq left right) = show left <> "\n" <> show right

{- This is an example of a DAG representing the following program:
   a = 5
   b = 4 + a
   print a
   print b -}
example1 :: DAG ()
example1 = (Assignment (Variable TInt "a") (Literal TInt 5))
  `Seq` (Assignment (Variable TInt "b")
        (BinOp TInt TInt TInt Add (Literal TInt 4) (Var (Variable TInt "a"))))
  `Seq` (PrintVal (Var (Variable TInt "a")))
  `Seq` (PrintVal (Var (Variable TInt "b")))

{- This is an example DAG representing the following program:
   a = 1
   b = 0
   if b == a {
     print 0
   }
   if a > b {
     print 1
   }
   print a + b -}
example2 :: DAG ()
example2 = (Assignment (Variable TInt "a") (Literal TInt 1))
  `Seq` (Assignment (Variable TInt "b") (Literal TInt 0))
  `Seq` (If (BinOp TInt TInt TBool Eq (Var (Variable TInt "b"))
             (Var (Variable TInt "a")))
        (PrintVal (Literal TInt 0))
        Pass)
  `Seq` (If (BinOp TInt TInt TBool Gt (Var (Variable TInt "a"))
             (Var (Variable TInt "b")))
        (PrintVal (Literal TInt 1))
        Pass)
  `Seq` (PrintVal (BinOp TInt TInt TInt Add (Var (Variable TInt "a"))
                   (Var (Variable TInt "b"))))
           

{- Print first 5 Fibonacci numbers (featuring variable shadowing)
   (TODO: use a loop instead):
   a = 1
   b = 1
   print a
   print b
   c = a + b
   a = b
   b = c
   print b
   c = a + b
   a = b
   b = c
   print b
   c = a + b
   a = b
   b = c
   print b
   c = a + b
   a = b
   b = c
   print b
   c = a + b
   a = b
   b = c
   print b -}
exampleFib :: DAG ()
exampleFib = (Assignment (Variable TInt "a") (Literal TInt 1))
  `Seq` (Assignment (Variable TInt "b") (Literal TInt 1))
  `Seq` (PrintVal (Var (Variable TInt "a")))
  `Seq` (PrintVal (Var (Variable TInt "b")))
  `Seq` (Assignment (Variable TInt "c")
        (BinOp TInt TInt TInt Add (Var (Variable TInt "a"))
         (Var (Variable TInt "b"))))
  `Seq` (Assignment (Variable TInt "a") (Var (Variable TInt "b")))
  `Seq` (Assignment (Variable TInt "b") (Var (Variable TInt "c")))
  `Seq` (PrintVal (Var (Variable TInt "b")))
  `Seq` (Assignment (Variable TInt "c")
        (BinOp TInt TInt TInt Add (Var (Variable TInt "a"))
         (Var (Variable TInt "b"))))
  `Seq` (Assignment (Variable TInt "a") (Var (Variable TInt "b")))
  `Seq` (Assignment (Variable TInt "b") (Var (Variable TInt "c")))
  `Seq` (PrintVal (Var (Variable TInt "b")))
  `Seq` (Assignment (Variable TInt "c")
        (BinOp TInt TInt TInt Add (Var (Variable TInt "a"))
         (Var (Variable TInt "b"))))
  `Seq` (Assignment (Variable TInt "a") (Var (Variable TInt "b")))
  `Seq` (Assignment (Variable TInt "b") (Var (Variable TInt "c")))
  `Seq` (PrintVal (Var (Variable TInt "b")))
  `Seq` (Assignment (Variable TInt "c")
        (BinOp TInt TInt TInt Add (Var (Variable TInt "a"))
         (Var (Variable TInt "b"))))
  `Seq` (Assignment (Variable TInt "a") (Var (Variable TInt "b")))
  `Seq` (Assignment (Variable TInt "b") (Var (Variable TInt "c")))
  `Seq` (PrintVal (Var (Variable TInt "b")))
  `Seq` (Assignment (Variable TInt "c")
        (BinOp TInt TInt TInt Add (Var (Variable TInt "a"))
         (Var (Variable TInt "b"))))
  `Seq` (Assignment (Variable TInt "a") (Var (Variable TInt "b")))
  `Seq` (Assignment (Variable TInt "b") (Var (Variable TInt "c")))
  `Seq` (PrintVal (Var (Variable TInt "b")))
