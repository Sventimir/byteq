{-# LANGUAGE DataKinds, GADTs #-}
module Data.Stack
  ( Stack
  , OnStack(..)
  , empty
  , push
  , pop
  , swap
  , dig
  ) where

import Data.Kind (Type)

{- The primary purpose of this data structure is to provide a
   framework for memory management. All intermediate values during
   computations will simply be put on stack and each instruction will
   automatically obtain its inputs from its top.  Additionally, this
   will also be used during compilation to model the memory and
   compute instructions to allocate and access variables from the
   stack.  The type variable stores the types of values stored on the
   stack. So that we can access it in a type-safe manner. We use
   DataKinds language extension to promote the list data structure to
   the type level and compute lists of types of values stored on the
   stack. -}
data Stack :: [Type] -> Type where
  Empty :: Stack '[]
  Item :: t -> Stack r -> Stack (t ': r)

empty :: Stack '[]
empty = Empty

push :: t -> Stack r -> Stack (t ': r)
push = Item

{- We use the fact that the Stack type is parametrised by the types of
   values stored on it, so that when accessing it we can be sure that:
   * there indeed is a value to access (stack is not empty)
   * the value has the right type. -}
pop :: Stack (t ': r) -> (t, Stack r)
pop (Item t s) = (t, s)

{- Here again we use types to ensure that the stack holds
   at least two values to swap. -}
swap :: Stack (t ': s ': r) -> Stack (s ': t ': r)
swap (Item t (Item s r)) = Item s (Item t r)

{- This is the type of a witness that a value of certain type is on
   the stack. In case the value is dug somewhere beneath the top, the
   witness allows us to access the value in type- safe manner without
   caring about the values on top of it.  The first parameter
   represents the type of the value in question. The second parameter
   is the type the stack will have when the value is removed. The
   final parameter is the current type of the stack. -}
data OnStack :: Type -> [Type] -> [Type] -> Type where
  OnTop :: OnStack t s (t ': s)
  Beneath :: OnStack a s r -> OnStack a (t ': s) (t ': r)

dig :: OnStack a s r -> Stack r -> (a, Stack s)
dig OnTop (Item a s) = (a, s)
dig (Beneath w) (Item t s) =
  let (a, s') = dig w s in
  (a, Item t s')
