{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS
import Data.Int
import Foreign.Ptr
import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Constant
import LLVM.AST.Global
import LLVM.CodeGenOpt
import LLVM.CodeModel
import LLVM.Context
import LLVM.Module
import LLVM.OrcJIT
import LLVM.PassManager
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
    { moduleName = "orc",
      moduleDefinitions = [defAdd]
    }

withTestModule :: AST.Module -> (LLVM.Module.Module -> IO a) -> IO a
withTestModule mod f = withContext $ \context -> withModuleFromAST context mod f

failInIO :: ExceptT String IO a -> IO a
failInIO = either fail return <=< runExceptT

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec {optLevel = Just 3}

eagerJit :: AST.Module -> IO ()
eagerJit amod = do
  withTestModule amod $ \mod -> do

    putStrLn "Original Module Assembly:"
    asm <- moduleLLVMAssembly mod
    BS.putStrLn asm

    optimized <- withPassManager passes $ flip runPassManager mod
    when optimized $ putStrLn "Optimized:"
    optasm <- moduleLLVMAssembly mod
    BS.putStrLn optasm

    withHostTargetMachine PIC LLVM.CodeModel.Default LLVM.CodeGenOpt.Default $ \tm ->
      withExecutionSession $ \es -> do
        let dylibName = "myDylib"
        dylib <- createJITDylib es dylibName
        withClonedThreadSafeModule mod $ \tsm -> do
          ol <- createRTDyldObjectLinkingLayer es
          il <- createIRCompileLayer es ol tm
          addModule tsm dylib il
          sym <- lookupSymbol es il dylib "add"
          case sym of
            Left (JITSymbolError err) -> do
              print err
            Right (JITSymbol mainFn _) -> do
              result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
              putStr "Eager JIT Result: "
              print result


main :: IO ()
main = do
  _res <- eagerJit module_
  return ()
