{-# LANGUAGE GADTs #-}
module Data.Type
  ( TType(..)
  , SomeType(..)
  ) where

import Data.Int (Int64)
import Data.Type.Equality ((:~:)(Refl), TestEquality(..))


{- This data structure binds Byteq's types to underlying Haskell
   types which they represent. It is used to build type-safe
   stack for memory-management and instruction set. -}
data TType a where
  TBool :: TType Bool
  TInt :: TType Int64 -- Use bounded Integer type for better performance.
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

instance Show (TType a) where
  show TBool = "Bool"
  show TInt = "Int"

instance Eq (TType a) where
  TBool == TBool = True
  TInt == TInt = True

{- This existential type wrapper is necessary in cases where we need to
   return a type, but we don't know in advance which one. This is because
   in this type the constructor determines the exact type, which maybe
   unknown in advance (for instance when parsing bytecode). It allows us
   hide the type variable and only access it through pattern-matching,
   when it becomes known (again, because the constructor determines the
   exact type). -}
data SomeType where
  SomeType :: TType a -> SomeType

instance Eq SomeType where
  SomeType TBool == SomeType TBool = True
  SomeType TInt == SomeType TInt = True
  _ == _ = False
