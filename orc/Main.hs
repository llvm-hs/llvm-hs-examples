{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import LLVM.AST
import LLVM.Module
import LLVM.Target
import LLVM.Context
import LLVM.AST.Global
import LLVM.AST.Constant
import qualified LLVM.AST as AST

import LLVM.OrcJIT

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS

import Data.Int
import Data.Word
import Foreign.Ptr

foreign import ccall "dynamic"
  mkMain :: FunPtr (IO Int32) -> IO Int32

int :: Type
int = IntegerType 32

defAdd :: Definition
defAdd = GlobalDefinition functionDefaults
  { name = Name "add"
  , parameters = ( [] , False )
  , returnType = int
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (Name "entry")
        []
        (Do $ Ret (Just (ConstantOperand (Int 32 42))) [])


module_ :: AST.Module
module_ = defaultModule
  { moduleName = "basic"
  , moduleDefinitions = [defAdd]
  }

withTestModule :: AST.Module -> (LLVM.Module.Module -> IO a) -> IO a
withTestModule mod f = withContext $ \context -> withModuleFromAST context mod f

resolver :: MangledSymbol -> IRCompileLayer l -> MangledSymbol -> IO JITSymbol
resolver testFunc compileLayer symbol
  = findSymbol compileLayer symbol True

nullResolver :: MangledSymbol -> IO JITSymbol
nullResolver s = return (JITSymbol 0 (JITSymbolFlags False False))

failInIO :: ExceptT String IO a -> IO a
failInIO = either fail return <=< runExceptT

eagerJit :: AST.Module -> IO Int32
eagerJit amod =
    withTestModule amod $ \mod ->
      withHostTargetMachine $ \tm ->
        withObjectLinkingLayer $ \objectLayer ->
          withIRCompileLayer objectLayer tm $ \compileLayer -> do
            asm <- moduleLLVMAssembly mod
            BS.putStrLn asm
            testFunc <- mangleSymbol compileLayer "add"
            withModuleSet
              compileLayer
              [mod]
              (SymbolResolver (resolver testFunc compileLayer) nullResolver) $
              \moduleSet -> do
                mainSymbol <- mangleSymbol compileLayer "add"
                JITSymbol mainFn _ <- findSymbol compileLayer mainSymbol True
                result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
                return result

main :: IO ()
main = do
  res <- eagerJit module_
  putStrLn "Eager JIT Result:"
  print res
