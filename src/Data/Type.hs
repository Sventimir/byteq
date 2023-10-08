{-# LANGUAGE GADTs #-}
module Data.Type
  ( TType(..)
  ) where

import Data.Type.Equality ((:~:)(Refl), TestEquality(..))


{- This data structure binds Byteq's types to underlying Haskell
   types which they represent. It is used to build type-safe
   stack for memory-management and instruction set. -}
data TType a where
  TBool :: TType Bool
  TInt :: TType Int -- Use bounded Integer type for better performance.
  -- TPair :: Type a -> Type b -> Type (a, b)
  -- TSum :: Type a -> Type b -> Type (Either a b)
  
{- TestEquality is class similar to Eq, but for parametric types. Not
   only does it check if given values are equal, but also provides a
   proof of type equality for their type parameters (called Refl). In
   the presence of this proof Haskell compiler can convince itself
   that these type parameters are equal and use this information for
   further type checking. -}
instance TestEquality TType where
  testEquality TBool TBool = Just Refl
  testEquality TInt TInt = Just Refl
  testEquality _ _ = Nothing
