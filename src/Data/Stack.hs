{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators #-}
module Data.Stack
  ( Stack(..)
  , OnStack(..)
  , StackItem(..)
  , empty
  , push
  , pop
  , swap
  , dupDig
  , dumpAddr
  , find
  ) where

import Data.Int (Int64)
import Data.ByteString.Builder (Builder, word8, int64BE)
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl), TestEquality(..))
import Data.Word (Word8)

{- To be store-able on a stack, a type must implement:
  * Show (for Print instructionn)
  * Eq (for Eq instruction)
  * toBytes (for encoding literals in bytecode). -}
class (Eq i, Show i) => StackItem i where
  encode :: i -> Builder

instance StackItem Int64 where
  encode = int64BE
  
instance StackItem Bool where
  encode True = word8 0
  encode False = word8 1

-- This is a dummy instance. We don't need to store () on the stack.
instance StackItem () where
  encode () = mempty
  
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
   stack.
   The type parameter f determines a precise data structure to be stored.
   for execution we want simple values, so this will be Identity, but
   in other instances we may want to store types or variables – in which
   case we parametrise Stack with the appropriate type constructor. -}
data Stack :: (Type -> Type) -> [Type] -> Type where
  Empty :: Stack f '[]
  Item :: StackItem t => f t -> Stack f r -> Stack f (t ': r)

{- TestEquality is class similar to Eq, but for parametric types. Not
   only does it check if given values are equal, but also provides a
   proof of type equality for their type parameters (called Refl). In
   the presence of this proof Haskell compiler can convince itself
   that these type parameters are equal and use this information for
   further type checking.
   Here we specify that Stack f can be tested for type equality IF
   f can be tested and we use that constrain to call testEquality
   on the stack items. This is important for compilation, where we
   occasionally need to test stacks for equality. -}
instance TestEquality f => TestEquality (Stack f) where
  testEquality Empty Empty = Just Refl
  testEquality (Item v1 s1) (Item v2 s2) = do
    Refl <- testEquality v1 v2
    Refl <- testEquality s1 s2
    return Refl
  testEquality _ _ = Nothing

empty :: Stack f '[]
empty = Empty

push :: StackItem t => f t -> Stack f r -> Stack f (t ': r)
push = Item

{- We use the fact that the Stack type is parametrised by the types of
   values stored on it, so that when accessing it we can be sure that:
   * there indeed is a value to access (stack is not empty)
   * the value has the right type. -}
pop :: Stack f (t ': r) -> (f t, Stack f r)
pop (Item t s) = (t, s)

{- Here again we use types to ensure that the stack holds
   at least two values to swap. -}
swap :: Stack f (t ': s ': r) -> Stack f (s ': t ': r)
swap (Item t (Item s r)) = Item s (Item t r)

{- This is the type of a witness that a value of certain type is on
   the stack. In case the value is dug somewhere beneath the top, the
   witness allows us to access the value in a type-safe manner without
   caring about the values on top of it.  The first parameter
   represents the type of the value in question. The second parameter
   is the type the stack will have when the value is removed. The
   final parameter is the current type of the stack. -}
data OnStack :: Type -> [Type] -> Type where
  OnTop :: OnStack t (t ': s)
  Beneath :: OnStack a r -> OnStack a (t ': r)

-- dig :: OnStack a s r -> Stack r -> (a, Stack s)
-- dig OnTop (Item a s) = (a, s)
-- dig (Beneath w) (Item t s) =
--   let (a, s') = dig w s in
--   (a, Item t s')

dupDig :: StackItem a => OnStack a r -> Stack f r -> Stack f (a ': r)
dupDig w s = Item (dig w s) s
  where
  dig :: OnStack a r -> Stack f r -> f a
  dig OnTop (Item a _) = a
  dig (Beneath w') (Item _ s') = dig w' s'

{- Convert an address to Integer form for byte encoding or display.
   NOTE: because we want every bytecode element to be constant-size,
   we need to decide on a constant-size type to be returned here.  We
   don't need sign, as these are effectively natural numbers, so let's
   use Word8. This has the side-effect of limitting the stack size to
   256 items. We should ensure stack cannot grow larger than that or
   else we will be producing wrong addresses. -}
dumpAddr :: OnStack a s -> Word8
dumpAddr OnTop = 0
dumpAddr (Beneath addr) = succ $ dumpAddr addr

{- Provided stack items are instances of TestEquality class, we can
   search the stack for a particular item and compute its address.
   Note that regular Eq class is too weak here, as we have to match
   the precise type of the item, or else GHC won't be able to infer
   it.  Also note that OnStack does not care about the items' type
   constructor, just the type parameter. This allows us to compute
   addresses on stacks of variables during compilation, but resolve
   them on stacks of values during execution.-}
find :: TestEquality f => f a -> Stack f s -> Maybe (OnStack a s)
find _ Empty = Nothing
find a (Item b s) =
  case testEquality a b of
    Just Refl -> Just OnTop
    Nothing -> Beneath <$> find a s
