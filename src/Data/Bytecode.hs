{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators #-}
module Data.Bytecode
  ( bytecode
  , dump
  , writeBytecode
  , parseInt64
  , parseBytecode
  ) where

import Data.Bits (shift, testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as Bytes
import Data.Instr (Instr(..), Seq(..), append)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Maybe (isJust)
import Data.Stack (Stack(..), StackItem(..), OnStack(..), dumpAddr)
import Data.Type (TType(..), SomeType(..))
import Data.Type.Equality ((:~:)(Refl), TestEquality(..))
import Data.Word (Word8)


{- Dump a sequence of instructions to a string (useful for debuggind). -}
dump :: Seq s t -> String
dump s = "[" <> intercalate "; " (eachInstr s) <> "]"
  where
  eachInstr :: Seq s t -> [String]
  eachInstr Halt = []
  eachInstr (instr :> s') = dumpInstr instr : eachInstr s'

dumpInstr :: Instr s t -> String
dumpInstr (Push t a) = "Push (" <> show a <> " : " <> show t <> ")"
dumpInstr Drop = "Drop"
dumpInstr Dup = "Dup"
dumpInstr Swap = "Swap"
dumpInstr (Dig addr) = "Dig " <> show (dumpAddr addr)
dumpInstr Add = "Add"
dumpInstr Sub = "Sub"
dumpInstr Mul = "Mul"
dumpInstr Div = "Div"
dumpInstr Rem = "Rem"
dumpInstr Eq = "Eq"
dumpInstr Lt = "Lt"
dumpInstr Gt = "Gt"
dumpInstr Not = "Not"
dumpInstr (Cond l r) = "Cond (" <> dump l <> ") (" <> dump r <> ")"
dumpInstr Print = "Print"

{- Convert a sequence of instructions to a bytestring. This creates a raw
   representation of an instruction sequence to be written to a file.
   This essentially renders a 0-terminated string of bytes. Several instructions
   need to store additional data (literals, memory addresses, nested sequences
   of instructions etc.). For this reason we're limited to 255 instruction codes,
   but we're far from reaching this limit in this exercise, so it's fine.
   Also note that a function parsing those bytestrings must be aware of which
   instructions store additional data and how to parse it. -}
bytecode :: Seq s t -> Bytes.Builder
bytecode Halt = Bytes.word8 0
bytecode (instr :> s) = opCode instr <> bytecode s

opCode :: Instr s t -> Bytes.Builder
opCode (Push t a) = Bytes.word8 1 <> encode t <> encode a
opCode Drop = Bytes.word8 2
opCode Dup = Bytes.word8 3
opCode Swap = Bytes.word8 4
opCode (Dig addr) = Bytes.word8 5 <> Bytes.word8 (dumpAddr addr)
opCode Add = Bytes.word8 6
opCode Sub = Bytes.word8 7
opCode Mul = Bytes.word8 8
opCode Div = Bytes.word8 9
opCode Rem = Bytes.word8 10
opCode Eq = Bytes.word8 11
opCode Lt = Bytes.word8 12
opCode Gt = Bytes.word8 13
opCode Not = Bytes.word8 14
-- Note that these sequences need to be parsed until 0 (Halt) is reached.
opCode (Cond l r) = Bytes.word8 15 <> bytecode l <> bytecode r
opCode Print = Bytes.word8 16

writeBytecode :: FilePath -> Seq s t -> IO ()
writeBytecode path = Bytes.writeFile path . bytecode

data BytecodeError where
  EndOfInput :: BytecodeError
  InvalidType :: Word8 -> BytecodeError
  InvalidBool :: Word8 -> BytecodeError
  InvalidOpcode :: Word8 -> BytecodeError
  UnexpectedEmptyStack :: BytecodeError
  TypeMismatch :: TType a -> TType b -> BytecodeError
  StackMismatch :: Stack TType a -> Stack TType b -> BytecodeError

instance Eq BytecodeError where
  EndOfInput == EndOfInput = True
  InvalidType w1 == InvalidType w2 = w1 == w2
  InvalidBool w1 == InvalidBool w2 = w1 == w2
  InvalidOpcode w1 == InvalidOpcode w2 = w1 == w2
  UnexpectedEmptyStack == UnexpectedEmptyStack = True
  TypeMismatch t1 t2 == TypeMismatch t3 t4 =
    isJust (testEquality t1 t3)
    && isJust (testEquality t2 t4)
  StackMismatch s1 s2 == StackMismatch s3 s4 =
    isJust (testEquality s1 s3)
    && isJust (testEquality s2 s4)
  _ == _ = False
  

data BytecodeParseState :: [Type] -> Type where
  BytecodeParseState :: ByteString -> Stack TType t -> Seq s t -> BytecodeParseState s

data StackAddrAndType s where
  StackAddrAndType :: StackItem t => TType t -> OnStack t s -> StackAddrAndType s

{- The entrypoint to the bytecode parsing machinery. -}
parseBytecode :: ByteString -> Either BytecodeError (BytecodeParseState '[])
parseBytecode bs = parseBC $ BytecodeParseState bs Empty Halt

{- When loading bytecode from a file, it is necessary to make sure it
   is well-formed, that is the types do match. The compiler ensures this,
   but when data is loaded from file, GHC does not know if the file was
   produced by the compiler or by something else. Therefore we need some
   type-checking to make sure this is the case. This slows VM's startup
   a little, but protects us from bytecode corruptions (e.g. during
   transfer over a network). -}
parseBC :: BytecodeParseState s -> Either BytecodeError (BytecodeParseState s)
parseBC (BytecodeParseState bs stack instr) = do
  (w, bs') <- uncons bs
  case w of
    0 -> Right $ BytecodeParseState bs' stack instr -- Halt is already there.
    1 -> do
      (t, bs'') <- parseTType bs'
      case t of
        SomeType TBool -> do
          (b, bs''') <- parseBool bs''
          parseBC
            (BytecodeParseState bs''' (Item TBool stack) (append instr $ Push TBool b))
        SomeType TInt -> do
          (i, bs''') <- parseInt64 bs''
          parseBC
            (BytecodeParseState bs''' (Item TInt stack) (append instr $ Push TInt i))
    2 -> do
      case stack of
        Empty -> Left UnexpectedEmptyStack
        Item _ stack' ->
          parseBC (BytecodeParseState bs' stack' (append instr Drop))
    3 -> do
      case stack of
        Empty -> Left UnexpectedEmptyStack
        Item t _ ->
          parseBC
          (BytecodeParseState bs' (Item t stack) (append instr Dup))
    4 -> do
      case stack of
        Empty -> Left UnexpectedEmptyStack
        Item _ Empty -> Left UnexpectedEmptyStack
        Item t1 (Item t2 stack') ->
          parseBC
          (BytecodeParseState bs' (Item t2 $ Item t1 stack') (append instr Swap))
    5 -> do
      (w', bs'') <- uncons bs'
      StackAddrAndType t addr <- parseAddr stack w'
      parseBC (BytecodeParseState bs'' (Item t stack) (append instr $ Dig addr))
    6 -> do
      case stack of
        Empty -> Left UnexpectedEmptyStack
        Item _ Empty -> Left UnexpectedEmptyStack
        Item TInt (Item TInt stack') ->
          parseBC (BytecodeParseState bs' (Item TInt stack') (append instr Add))
        Item TInt (Item t _) ->
          Left $ TypeMismatch TInt t
        Item t _ ->
          Left $ TypeMismatch TInt t
    7 -> do
      case stack of
        Empty -> Left UnexpectedEmptyStack
        Item _ Empty -> Left UnexpectedEmptyStack
        Item TInt (Item TInt stack') ->
          parseBC (BytecodeParseState bs' (Item TInt stack') (append instr Sub))
        Item TInt (Item t _) ->
          Left $ TypeMismatch TInt t
        Item t _ ->
          Left $ TypeMismatch TInt t
    8 -> do
      case stack of
        Empty -> Left UnexpectedEmptyStack
        Item _ Empty -> Left UnexpectedEmptyStack
        Item TInt (Item TInt stack') ->
          parseBC (BytecodeParseState bs' (Item TInt stack') (append instr Mul))
        Item TInt (Item t _) ->
          Left $ TypeMismatch TInt t
        Item t _ ->
          Left $ TypeMismatch TInt t
    9 -> do
      case stack of
        Empty -> Left UnexpectedEmptyStack
        Item _ Empty -> Left UnexpectedEmptyStack
        Item TInt (Item TInt stack') ->
          parseBC (BytecodeParseState bs' (Item TInt stack') (append instr Div))
        Item TInt (Item t _) ->
          Left $ TypeMismatch TInt t
        Item t _ ->
          Left $ TypeMismatch TInt t
    10 -> do
      case stack of
        Empty -> Left UnexpectedEmptyStack
        Item _ Empty -> Left UnexpectedEmptyStack
        Item TInt (Item TInt stack') ->
          parseBC (BytecodeParseState bs' (Item TInt stack') (append instr Rem))
        Item TInt (Item t _) ->
          Left $ TypeMismatch TInt t
        Item t _ ->
          Left $ TypeMismatch TInt t
    11 ->
      case stack of
        Empty -> Left UnexpectedEmptyStack
        Item _ Empty -> Left UnexpectedEmptyStack
        Item t1 (Item t2 stack') -> do
          Refl <- assertTypesMatch t1 t2
          parseBC (BytecodeParseState bs' (Item TBool stack') (append instr Eq))
    12 ->
      case stack of
        Empty -> Left UnexpectedEmptyStack
        Item _ Empty -> Left UnexpectedEmptyStack
        Item TInt (Item TInt stack') ->
          parseBC (BytecodeParseState bs' (Item TBool stack') (append instr Lt))
        Item TInt (Item t _) ->
          Left $ TypeMismatch TInt t
        Item t _ ->
          Left $ TypeMismatch TInt t
    13 -> 
      case stack of
        Empty -> Left UnexpectedEmptyStack
        Item _ Empty -> Left UnexpectedEmptyStack
        Item TInt (Item TInt stack') ->
          parseBC (BytecodeParseState bs' (Item TBool stack') (append instr Lt))
        Item TInt (Item t _) ->
          Left $ TypeMismatch TInt t
        Item t _ ->
          Left $ TypeMismatch TInt t
    14 ->
      case stack of
        Empty -> Left UnexpectedEmptyStack
        Item TBool _ ->
          parseBC (BytecodeParseState bs' stack (append instr Not))
        Item t _ ->
          Left $ TypeMismatch TBool t
    15 ->
      case stack of
        Empty -> Left UnexpectedEmptyStack
        Item TBool stack' -> do
          BytecodeParseState bs'' thenStack thenInstr <- parseBC (BytecodeParseState bs' stack' Halt)
          BytecodeParseState bs''' elseStack elseInstr <- parseBC (BytecodeParseState bs'' stack' Halt)
          Refl <- assertStacksMatch thenStack elseStack
          parseBC $ BytecodeParseState bs''' thenStack (append instr $ Cond thenInstr elseInstr)
        Item t _ ->
          Left $ TypeMismatch TBool t
    16 ->
      case stack of
        Empty -> Left UnexpectedEmptyStack
        Item _ stack' -> do
          parseBC $ BytecodeParseState bs' stack' (append instr Print)
    _ -> Left $ InvalidOpcode w
          
{- Parse first 8 bytes from the bytestring as a signed Int64. -}
parseInt64 :: ByteString -> Either BytecodeError (Int64, ByteString)
parseInt64 bs =
  let (i, t) = B.splitAt 8 bs in
  if B.length i < 8
    then Left EndOfInput
    else Right (snd . foldr toInt64 (0, 0) $ B.unpack i, t)
  where
  signed :: Word8 -> Int64 -> Int64
  signed w i = if testBit w 7 then -i else i
  toInt64 :: Word8 -> (Int, Int64) -> (Int, Int64)
  toInt64 w (i, acc)
    | i > 63 = (64, signed w (acc + (fromIntegral w `shift` 63)))
    | otherwise = (i + 8, acc + (fromIntegral w `shift` i))

{- Parse a type tag. Ideally, we would like to avoid storing type information
   explicitly in the bytecode, but sometimes it is necessary. For example
   a Push instruction is followed by a value to push. Without a type tag,
   the VM would have no way of knowing how to interpret this value, or indeed.
   how much of the bytestring it should parse, as integers occupy 8 bytes each,
   whiole booleans – just a single byte. -}
parseTType :: ByteString -> Either BytecodeError (SomeType, ByteString)
parseTType bs = do
  (w, bs') <- uncons bs
  case w of
    0 -> Right (SomeType TBool, bs')
    1 -> Right (SomeType TInt, bs')
    _ -> Left $ InvalidType w

{- Parse a boolean value. -}
parseBool :: ByteString -> Either BytecodeError (Bool, ByteString)
parseBool bs = do
  (w, bs') <- uncons bs
  case w of
    0 -> Right (False, bs')
    1 -> Right (True, bs')
    _ -> Left $ InvalidBool w

{- In the bytecode an address is basically an unsigned 1-byte integer. However,
   the internal representation of the address is typed to ensure that it finds
   the right value on the stack. For this reason, this function must recover this
   type information from the context, that is from the current type of the stack
   maintained by the parser. -}
parseAddr :: Stack TType s -> Word8 -> Either BytecodeError (StackAddrAndType s)
parseAddr Empty _ = Left UnexpectedEmptyStack
parseAddr (Item t _) 0 = Right $ StackAddrAndType t OnTop
parseAddr (Item _ stack) w = do
  StackAddrAndType t addr <- parseAddr stack (pred w)
  return . StackAddrAndType t $ Beneath addr

assertTypesMatch :: TType a -> TType b -> Either BytecodeError (a :~: b)
assertTypesMatch t1 t2 =
  maybe (Left $ TypeMismatch t1 t2) Right $ testEquality t1 t2

assertStacksMatch :: Stack TType s -> Stack TType t -> Either BytecodeError (s :~: t)
assertStacksMatch s t =
  maybe (Left $ StackMismatch s t) Right $ testEquality s t

uncons :: ByteString -> Either BytecodeError (Word8, ByteString)
uncons bs =
  case B.uncons bs of
    Nothing -> Left EndOfInput
    Just (w, bs') -> Right (w, bs')

