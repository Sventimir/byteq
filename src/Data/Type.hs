{-# LANGUAGE GADTs #-}
module Data.Type
  ( TType(..)
  ) where


{- This data structure binds Byteq's types to underlying Haskell
   types which they represent. It is used to build type-safe
   stack for memory-management and instruction set. -}
data TType a where
  TBool :: TType Bool
  TInt :: TType Int -- Use bounded Integer type for better performance.
  -- TPair :: Type a -> Type b -> Type (a, b)
  -- TSum :: Type a -> Type b -> Type (Either a b)
  
