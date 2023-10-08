{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators #-}
module Control.Compiler
  ( compile
  ) where

import Data.Kind (Type)
import Data.Type (TType(..))
import Data.Type.Equality ((:~:)(Refl), TestEquality(..))
import Data.DAG (DAG(..), Variable(..))
import Data.Instr (Instr(Push, Dig, Cond, Print), Seq(..), append)
import Data.Stack (OnStack(..), StackItem)


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

{- Search the VarStack for a given variable. Return the stack
   position if found; otherwise return Nothing. -}
find :: Variable a -> VarStack s -> Maybe (OnStack a s)
find _ Empty = Nothing
find v (Item v' s) =
  case testEquality v v' of
    Just Refl -> Just OnTop
    Nothing -> Beneath <$> find v s
  

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

{- This is the main function of the compiler. It takes a DAG and
   produces a sequence of instructions. -}
compile :: DAG () -> Either CompileError (Seq '[] '[Int])
compile dag = do
  CompilationResult finStack instr <- compileExpr dag Empty Halt
  case finStack of
    Item (Variable TInt _) Empty -> return instr
    _ -> Left $ TypeMismatch Nothing (Just TInt) finStack

{- NOTE: type parameter on DAG does not matter here, because
   expressions only exist at the DAG level. During compilation they're
   translated into sequences of instructions. This disconnection
   between expression types and stack types is an imperfection we
   might correct later. It's not critical, though. -}
compileExpr :: DAG a -> VarStack s -> Seq r s -> Either CompileError (CompilationResult r)
compileExpr (Literal typ val) stack iseq =
  return $ CompilationResult
      (Item (Variable typ "") stack)
      (append iseq $ Push val) 

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
    Just address -> return $ CompilationResult
                              (Item v stack)
                              (append instr $ Dig address)

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
