{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators #-}
module Data.VariableStack
  ( VarStack(..)
  , find
  ) where

import Data.DAG (Variable(..))
import Data.List (intercalate)
import Data.Kind(Type)
import Data.Stack (OnStack(..), StackItem)
import Data.Type.Equality (TestEquality(..), (:~:)(Refl))

{- This structure models a runtime stack. It contains typed variables,
   which can be referenced at runtime. When such a reference is found
   in the DAG, the stack is searched for a variable with the given
   name and type, and an address is built, describing how to access
   the variable at runtime.
   NOTE that this type reflects exactly the structure of the Stack
   from Data.Stack, but must be kept separate, because it holds
   variables rather than values. It is possible to unify them, but
   it's a lot of advanced type juggling, and the gain is not worth
   the effort. The compiler will be type-safe without it. -}
data VarStack :: [Type] -> Type where
  Empty :: VarStack '[]
  Item :: StackItem a => Variable a -> VarStack s -> VarStack (a ': s)

{- TestEquality is class similar to Eq, but for parametric types. Not
   only does it check if given values are equal, but also provides a
   proof of type equality for their type parameters (called Refl). In
   the presence of this proof Haskell compiler can convince itself
   that these type parameters are equal and use this information for
   further type checking. -}
instance TestEquality VarStack where 
  testEquality Empty Empty = Just Refl
  testEquality (Item v1 s1) (Item v2 s2) = do
    Refl <- testEquality v1 v2
    Refl <- testEquality s1 s2
    return Refl
  testEquality _ _ = Nothing

instance Show (VarStack s) where
  show stack = "[" <> intercalate ", " (showEach stack) <> "]"
    where
    showEach :: VarStack s -> [String]
    showEach Empty = []
    showEach (Item v s) = show v : showEach s

{- Search the VarStack for a given variable. Return the stack
   position if found; otherwise return Nothing. -}
find :: Variable a -> VarStack s -> Maybe (OnStack a s)
find _ Empty = Nothing
find v (Item v' s) =
  case testEquality v v' of
    Just Refl -> Just OnTop
    Nothing -> Beneath <$> find v s
  
