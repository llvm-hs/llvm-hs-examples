{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.Lazy.IO as T

import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as P

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction

simple :: Module
simple = buildModule "exampleModule" $ mdo
  function "f" [(AST.i32, "a")] AST.i32 $ \[a] -> mdo
    _entry <- block `named` "entry"
    cond <- icmp P.EQ a (ConstantOperand (C.Int 32 0))
    condBr cond ifThen ifElse
    ifThen <- block
    trVal <- add a (ConstantOperand (C.Int 32 0))
    br ifExit
    ifElse <- block `named` "if.else"
    flVal <- add a (ConstantOperand (C.Int 32 0))
    br ifExit
    ifExit <- block `named` "if.exit"
    r <- phi [(trVal, ifThen), (flVal, ifElse)]
    ret r

  function "plus" [(AST.i32, "x"), (AST.i32, "y")] AST.i32 $ \[x, y] -> do
    _entry <- block `named` "entry2"
    r <- add x y
    ret r

main :: IO ()
main = print simple
