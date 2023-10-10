{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, KindSignatures, TypeOperators #-}
module Control.Compiler
  ( CompilationResult(..)
  , compile
  ) where

import Data.DAG (DAG(..), Variable(..))
import Data.Instr (Instr(Push, Dig, Dup, Cond, Print), Seq(..), append)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Stack (Stack(..), OnStack(..), find)
import Data.Type (TType(..))
import Data.Type.Equality ((:~:)(Refl), TestEquality(..))


{- This is a stack type specialised to hold variables. It can be
   made an instance of show, because TType's type parameter is
   phantom: there's no actual value of that type inside. -}
type VarStack = Stack Variable

instance Show (VarStack s) where
  show stack = "[" <> intercalate ", " (showEach stack) <> "]"
    where
    showEach :: VarStack s -> [String]
    showEach Empty = []
    showEach (Item v s) = show v : showEach s

{- This structure represents a successful compilation step, where
   a sequence of instructions is returned, along with the type of
   the stack *after* its execution. This output stack type is then
   used as input for the next compilation step. -}
data CompilationResult s where
  CompilationResult :: VarStack t -> Seq s t -> CompilationResult s
  
data CompileError where
  Undefined :: Variable a -> CompileError
  TypeMismatch :: Maybe (DAG a) -> Maybe (TType a) -> VarStack s -> CompileError
  StackMismatch :: VarStack a -> VarStack b -> CompileError
  IlldefinedVar :: Maybe (Variable a) -> Variable b -> CompileError

instance Show CompileError where
  show (Undefined v) =
    "Undefined variable: " <> show v
  show (TypeMismatch expr tOpt stack) =
    let prog = fromMaybe "" (show <$> expr) in
    case tOpt of
      Just t ->
        "A value of type " <> show t <> " was expected, but " <>
        show stack <> " was found instead.\n" <> prog
      Nothing ->
        " A value was expected on stack, but it's empty.\n" <> prog
  show (StackMismatch expectedStack actualStack) =
    "A stack of shape: " <> show expectedStack <> " was expected, " <>
    "but: " <> show actualStack <> " was found instead."
  show (IlldefinedVar actualOpt expected) =
    case actualOpt of
      Nothing ->
        "A var binding requires a variable: " <> show expected <>
        ", but none was found."
      Just actual ->
        "A var binding expected a variable: " <> show expected <>
        ", but " <> show actual <> " was found."

{- This is the main function of the compiler. It takes a DAG and
   produces a sequence of instructions. -}
compile :: DAG () -> Either CompileError (CompilationResult '[])
compile dag = do compileExpr dag Empty Halt

{- NOTE: type parameter on DAG does not matter here, because
   expressions only exist at the DAG level. During compilation they're
   translated into sequences of instructions. This disconnection
   between expression types and stack types is an imperfection we
   might correct later. It's not critical, though. -}
compileExpr :: DAG a -> VarStack s -> Seq r s -> Either CompileError (CompilationResult r)
compileExpr (Literal typ val) stack iseq =
  return $ CompilationResult
      (Item (Variable typ "") stack)
      (append iseq $ Push typ val) 

compileExpr (Assignment var@(Variable typ name) expr) stack instr = do
  CompilationResult stack' instr' <- compileExpr expr stack instr
  case stack' of
    Empty -> Left $ IlldefinedVar Nothing var
    Item v@(Variable t _) remStack ->
      case testEquality t typ of
           Just Refl -> return $ CompilationResult
                           (Item (Variable t name) remStack)
                           instr'
           Nothing -> Left $ IlldefinedVar (Just v) var

compileExpr (Var v) stack instr =
  case find v stack of
    Nothing -> Left $ Undefined v
    -- The variable is already on top. We just need to Dup it.
    -- This saves us the necessity to store the address.
    Just OnTop -> return $ CompilationResult
                            (Item v stack)
                            (append instr Dup)
    Just address -> return $ CompilationResult
                              (Item v stack)
                              (append instr $ Dig address)

compileExpr Pass stack instr = return $ CompilationResult stack instr

compileExpr (UnOp tArg tRet i expr) stack instr = do
  CompilationResult stack' instr' <- compileExpr expr stack instr
  case stack' of
    Empty -> Left $ TypeMismatch (Just expr) (Just tArg) Empty
    Item (Variable top _) remStack ->
      case testEquality tArg top of
        Nothing -> Left $ TypeMismatch (Just expr) (Just tArg) stack'
        Just Refl -> return $ CompilationResult
                              (Item (Variable tRet "") remStack)
                              (append instr' i)

compileExpr (BinOp tLeft tRight tRet i left right) stack instr = do
  CompilationResult stack' instr' <- compileExpr right stack instr
  CompilationResult stack'' instr'' <- compileExpr left stack' instr'
  case stack'' of
    Empty -> Left $ TypeMismatch (Just left) (Just tLeft) Empty
    Item (Variable _ _) Empty -> Left $ TypeMismatch (Just right) (Just tRight) stack''
    Item (Variable top _) (Item (Variable subTop _) remStack) ->
      case testEquality tLeft top of
        Nothing -> Left $ TypeMismatch (Just left) (Just tLeft) stack''
        Just Refl ->
          case testEquality tRight subTop of
            Nothing -> Left $ TypeMismatch (Just right) (Just tRight) stack''
            Just Refl -> return $ CompilationResult
                              (Item (Variable tRet "") remStack)
                              (append instr'' i)
  
compileExpr (If cond left right) stack instr = do
  CompilationResult stack' instr' <- compileExpr cond stack instr
  case stack' of
    Empty -> Left $ TypeMismatch (Just cond) (Just TBool) Empty
    Item (Variable TBool _) remStack -> do
      CompilationResult thenStack thenInstr <- compileExpr left remStack Halt
      CompilationResult elseStack elseInstr <- compileExpr right remStack Halt
      case testEquality thenStack elseStack of
        Nothing -> Left $ StackMismatch thenStack elseStack
        Just Refl ->
          return $ CompilationResult
                     thenStack
                     (append instr' $ Cond thenInstr elseInstr)
    _ -> Left $ TypeMismatch (Just cond) (Just TBool) stack'

compileExpr (PrintVal expr) stack instr = do
  CompilationResult stack' instr' <- compileExpr expr stack instr
  case stack' of
    Empty -> Left $ TypeMismatch (Just expr) Nothing Empty
    Item _ remStack ->
      return $ CompilationResult
      remStack
      (append instr' Print)

compileExpr (Seq left right) stack instr = do
  CompilationResult stack' instr' <- compileExpr left stack instr
  CompilationResult stack'' instr'' <- compileExpr right stack' instr'
  return $ CompilationResult stack'' instr''
