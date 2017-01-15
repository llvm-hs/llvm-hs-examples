module Main where

import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module

import Control.Monad.Except

int :: Type
int = IntegerType 32

defAdd :: Definition
defAdd = GlobalDefinition functionDefaults
  { name = Name "add"
  , parameters =
      ( [ Parameter int (Name "a") []
        , Parameter int (Name "b") [] ]
      , False )
  , returnType = int
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (Name "entry")
        [ Name "result" :=
            Add False  -- no signed wrap
                False  -- no unsigned wrap
                (LocalReference int (Name "a"))
                (LocalReference int (Name "b"))
                []]
        (Do $ Ret (Just (LocalReference int (Name "result"))) [])


module_ :: AST.Module
module_ = defaultModule
  { moduleName = "basic"
  , moduleDefinitions = [defAdd]
  }


toLLVM :: AST.Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
    errOrLLVM <- runExceptT $ withModuleFromAST ctx mod moduleLLVMAssembly
    case errOrLLVM of
      Left err -> putStrLn $ "error: " ++ err
      Right llvm -> putStrLn llvm


main :: IO ()
main = toLLVM module_
