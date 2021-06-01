{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.Int
import qualified Data.Map.Strict as Map
import Foreign.Ptr
import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Constant
import LLVM.AST.Global
import LLVM.CodeGenOpt
import LLVM.CodeModel
import LLVM.Context
import LLVM.Internal.OrcJIT.CompileLayer
import LLVM.Module
import LLVM.OrcJIT
import LLVM.Relocation
import LLVM.Target
import Prelude hiding (mod)

foreign import ccall "dynamic"
  mkMain :: FunPtr (IO Int32) -> IO Int32

int :: Type
int = IntegerType 32

defAdd :: Definition
defAdd =
  GlobalDefinition
    functionDefaults
      { name = Name "add",
        parameters = ([], False),
        returnType = int,
        basicBlocks = [body]
      }
  where
    body =
      BasicBlock
        (Name "entry")
        []
        (Do $ Ret (Just (ConstantOperand (Int 32 42))) [])

module_ :: AST.Module
module_ =
  defaultModule
    { moduleName = "basic",
      moduleDefinitions = [defAdd]
    }

withTestModule :: AST.Module -> (LLVM.Module.Module -> IO a) -> IO a
withTestModule mod f = withContext $ \context -> withModuleFromAST context mod f

resolver :: CompileLayer l => l -> MangledSymbol -> IO (Either JITSymbolError JITSymbol)
resolver compileLayer symbol = findSymbol compileLayer symbol True

failInIO :: ExceptT String IO a -> IO a
failInIO = either fail return <=< runExceptT

eagerJit :: AST.Module -> IO ()
eagerJit amod = do
  resolvers <- newIORef Map.empty
  withTestModule amod $ \mod ->
    withHostTargetMachine PIC LLVM.CodeModel.Default LLVM.CodeGenOpt.Default $ \tm ->
      withExecutionSession $ \es ->
        withObjectLinkingLayer es (\k -> fmap (\rs -> rs Map.! k) (readIORef resolvers)) $ \linkingLayer ->
          withIRCompileLayer linkingLayer tm $ \compileLayer -> do
            mainSymbol <- mangleSymbol compileLayer "add"
            asm <- moduleLLVMAssembly mod
            BS.putStrLn asm
            withModuleKey es $ \k ->
              withSymbolResolver es (SymbolResolver (resolver compileLayer)) $ \sresolver -> do
                modifyIORef' resolvers (Map.insert k sresolver)
                withModule compileLayer k mod $ do
                  rsym <- findSymbol compileLayer mainSymbol True
                  case rsym of
                    Left err -> do
                      print err
                    Right (JITSymbol mainFn _) -> do
                      result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
                      print result

main :: IO ()
main = do
  res <- eagerJit module_
  putStrLn "Eager JIT Result:"
  print res
