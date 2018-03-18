{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Control.DeepSeq (NFData, force)
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Functor.Foldable hiding (fold)
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Text.Lazy (Text)
import Foreign.Ptr

import qualified Data.ByteString.Char8      as BS
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import qualified Data.Text.Lazy.IO          as Text
import qualified LLVM.AST                   as LLVM
import qualified LLVM.AST.Constant          as LLVM
import qualified LLVM.AST.Float             as LLVM
import qualified LLVM.AST.Type              as LLVM
import qualified LLVM.IRBuilder.Instruction as LLVMIR
import qualified LLVM.IRBuilder.Module      as LLVMIR
import qualified LLVM.IRBuilder.Monad       as LLVMIR
import qualified LLVM.Pretty                as LLVMPretty
import qualified LLVM.Context               as LLVMJIT
import qualified LLVM.Linking               as LLVMJIT
import qualified LLVM.Module                as LLVMJIT
import qualified LLVM.OrcJIT                as LLVMJIT
import qualified LLVM.Target                as LLVMJIT

-- * Core expression type

-- | An expression will be any value of type @'Fix' 'ExprF'@, which
--   has as values arbitrarily nested applications of constructors from
--   'ExprF'. This is equivalent to just having an 'Expr type with no type
--   parameter and all @a@s replaced by 'Expr', but the 'Functor' and 'Foldable'
--   instances are quite handy, especially combined with the /recursion-schemes/
--   library.
--
--   This type allows us to express the body of a @Double -> Double@ function,
--   where 'Var' allows us to refer to the (only) argument of the function.
data ExprF a
  = Lit Double -- ^ a 'Double' literal
  | Add a a    -- ^ @a+b@
  | Sub a a    -- ^ @a-b@
  | Mul a a    -- ^ @a*b@
  | Div a a    -- ^ @a/b@
  | Neg a      -- ^ @-a@
  | Exp a      -- ^ @'exp' a@
  | Log a      -- ^ @'log' a@
  | Sqrt a     -- ^ @'sqrt' a@
  | Sin a      -- ^ @'sin' a@
  | Cos a      -- ^ @'cos' a@
  | Var        -- ^ @'x'@
  deriving (Functor, Foldable, Traversable)

type Expr = Fix ExprF

-- * Helpers for building expressions

x :: Expr
x = Fix Var

lit :: Double -> Expr
lit d = Fix (Lit d)

add :: Expr -> Expr -> Expr
add a b = Fix (Add a b)

sub :: Expr -> Expr -> Expr
sub a b = Fix (Sub a b)

mul :: Expr -> Expr -> Expr
mul a b = Fix (Mul a b)

neg :: Expr -> Expr
neg a = Fix (Neg a)

instance Num Expr where
  fromInteger = lit . fromInteger
  (+)         = add
  (-)         = sub
  (*)         = mul
  negate      = neg
  abs         = notImplemented "Expr.abs"
  signum      = notImplemented "Expr.signum"

divide :: Expr -> Expr -> Expr
divide a b = Fix (Div a b)

instance Fractional Expr where
  (/)          = divide
  recip        = divide 1
  fromRational = lit . fromRational

instance Floating Expr where
  pi    = lit pi
  exp   = Fix . Exp
  log   = Fix . Log
  sqrt  = Fix . Sqrt
  sin   = Fix . Sin
  cos   = Fix . Cos
  asin  = notImplemented "Expr.asin"
  acos  = notImplemented "Expr.acos"
  atan  = notImplemented "Expr.atan"
  sinh  = notImplemented "Expr.sinh"
  cosh  = notImplemented "Expr.cosh"
  asinh = notImplemented "Expr.asinh"
  acosh = notImplemented "Expr.acosh"
  atanh = notImplemented "Expr.atanh"

notImplemented :: String -> a
notImplemented = error . (++ " is not implemented")

-- * Pretty printing

-- | Pretty print an 'Expr'
pp :: Expr -> String
pp e = funprefix ++ para ppExpAlg e

 where funprefix = "\\x -> "

printExpr :: MonadIO m => Expr -> m ()
printExpr expr = liftIO $ do
  putStrLn "*** Expression ***\n"
  putStrLn (pp expr)

-- | Core pretty printing function. For each
--   constructor that contains sub expressions,
--   we get the string for the sub expression as
--   well as the original 'Expr' value, to help us
--   decide when to use parens.
ppExpAlg :: ExprF (Expr, String) -> String
ppExpAlg (Lit d) = show d
ppExpAlg (Add (_, a) (_, b)) = a ++ " + " ++ b
ppExpAlg (Sub (_, a) (e2, b)) =
  a ++ " - " ++ paren (isAdd e2 || isSub e2) b
ppExpAlg (Mul (e1, a) (e2, b)) =
  paren (isAdd e1 || isSub e1) a ++ " * " ++ paren (isAdd e2 || isSub e2) b
ppExpAlg (Div (e1, a) (e2, b)) =
  paren (isAdd e1 || isSub e1) a ++ " / " ++ paren (isComplex e2) b

  where isComplex (Fix (Add _ _)) = True
        isComplex (Fix (Sub _ _)) = True
        isComplex (Fix (Mul _ _)) = True
        isComplex (Fix (Div _ _)) = True
        isComplex               _ = False

ppExpAlg (Neg (_, a)) = function "negate" a
ppExpAlg (Exp (_, a)) = function "exp" a
ppExpAlg (Log (_, a)) = function "log" a
ppExpAlg (Sqrt (_, a)) = function "sqrt" a
ppExpAlg (Sin (_, a)) = function "sin" a
ppExpAlg (Cos (_, a)) = function "cos" a
ppExpAlg Var = "x"

paren :: Bool -> String -> String
paren b x
  | b         = "(" ++ x ++ ")"
  | otherwise = x

function name arg =
  name ++ paren True arg

isAdd :: Expr -> Bool
isAdd (Fix (Add _ _)) = True
isAdd               _ = False

isSub :: Expr -> Bool
isSub (Fix (Sub _ _)) = True
isSub               _ = False

isLit :: Expr -> Bool
isLit (Fix (Lit _)) = True
isLit             _ = False

isVar :: Expr -> Bool
isVar (Fix Var) = True
isVar         _ = False

-- * Simple evaluator

-- | Evaluate an 'Expr'ession using standard
--   'Num', 'Fractional' and 'Floating' operations.
eval :: Expr -> (Double -> Double)
eval fexpr x = cata alg fexpr

  where alg e = case e of
          Var     -> x
          Lit d   -> d
          Add a b -> a + b
          Sub a b -> a - b
          Mul a b -> a * b
          Div a b -> a / b
          Neg a   -> negate a
          Exp a   -> exp a
          Log a   -> log a
          Sqrt a  -> sqrt a
          Sin a   -> sin a
          Cos a   -> cos a

-- * Code generation

-- | Helper for calling intrinsics for 'exp', 'log' and friends.
callDblfun
  :: LLVMIR.MonadIRBuilder m => LLVM.Operand -> LLVM.Operand -> m LLVM.Operand
callDblfun fun arg = LLVMIR.call fun [(arg, [])]

xparam :: LLVMIR.ParameterName
xparam = LLVMIR.ParameterName "x"

-- | Generate @declare@ statements for all the intrinsics required for
--   executing the given expression and return a mapping from function
--   name to 'Operand' so that we can very easily refer to those functions
--   for calling them, when generating the code for the expression itself.
declarePrimitives
  :: LLVMIR.MonadModuleBuilder m => Expr -> m (Map String LLVM.Operand)
declarePrimitives expr = fmap Map.fromList $
  forM primitives $ \primName -> do
    f <- LLVMIR.extern (LLVM.mkName ("llvm." <> primName <> ".f64"))
                       [LLVM.double]
                       LLVM.double
    return (primName, f)

  where primitives = Set.toList (cata alg expr)
        alg (Exp ps)  = Set.insert "exp" ps
        alg (Log ps)  = Set.insert "log" ps
        alg (Sqrt ps) = Set.insert "sqrt" ps
        alg (Sin ps)  = Set.insert "sin" ps
        alg (Cos ps)  = Set.insert "cos" ps
        alg e         = fold e

-- | Generate an LLVM IR module for the given expression,
--   including @declare@ statements for the intrinsics and
--   a function, always called @f@, that will perform the copoutations
--   described by the 'Expr'ession.
codegen :: Expr -> LLVM.Module
codegen fexpr = LLVMIR.buildModule "arith.ll" $ do
  prims <- declarePrimitives fexpr
  _ <- LLVMIR.function "f" [(LLVM.double, xparam)] LLVM.double $ \[arg] -> do
    res <- cataM (alg arg prims) fexpr
    LLVMIR.ret res
  return ()

  where alg arg _ (Lit d) =
          return (LLVM.ConstantOperand $ LLVM.Float $ LLVM.Double d)
        alg arg _ Var = return arg
        alg arg _ (Add a b) = LLVMIR.fadd a b `LLVMIR.named` "x"
        alg arg _ (Sub a b) = LLVMIR.fsub a b `LLVMIR.named` "x"
        alg arg _ (Mul a b) = LLVMIR.fmul a b `LLVMIR.named` "x"
        alg arg _ (Div a b) = LLVMIR.fdiv a b `LLVMIR.named` "x"
        alg arg ps (Neg a) = do
          z <- alg arg ps (Lit 0)
          LLVMIR.fsub z a `LLVMIR.named` "x"
        alg arg ps (Exp a) = callDblfun (ps Map.! "exp") a `LLVMIR.named` "x"
        alg arg ps (Log a) = callDblfun (ps Map.! "log") a `LLVMIR.named` "x"
        alg arg ps (Sqrt a) = callDblfun (ps Map.! "sqrt") a `LLVMIR.named` "x"
        alg arg ps (Sin a) = callDblfun (ps Map.! "sin") a `LLVMIR.named` "x"
        alg arg ps (Cos a) = callDblfun (ps Map.! "cos") a `LLVMIR.named` "x"

codegenText :: Expr -> Text
codegenText = LLVMPretty.ppllvm . codegen

printCodegen :: Expr -> IO ()
printCodegen = Text.putStrLn . codegenText

-- * JIT compilation & loading

-- | This allows us to call dynamically loaded functions
foreign import ccall "dynamic"
  mkDoubleFun :: FunPtr (Double -> Double) -> (Double -> Double)

resolver
  :: LLVMJIT.IRCompileLayer l -> LLVMJIT.MangledSymbol -> IO LLVMJIT.JITSymbol
resolver compileLayer symbol
  = LLVMJIT.findSymbol compileLayer symbol True

symbolFromProcess :: LLVMJIT.MangledSymbol -> IO LLVMJIT.JITSymbol
symbolFromProcess sym = (\addr -> LLVMJIT.JITSymbol addr (LLVMJIT.JITSymbolFlags False True))
    <$> LLVMJIT.getSymbolAddressInProcess sym

resolv :: LLVMJIT.IRCompileLayer l -> LLVMJIT.SymbolResolver
resolv cl = LLVMJIT.SymbolResolver (\sym -> LLVMJIT.findSymbol cl sym True) symbolFromProcess

printIR :: MonadIO m => ByteString -> m ()
printIR = liftIO . BS.putStrLn . ("\n*** LLVM IR ***\n\n"<>)

-- | JIT-compile the given 'Expr'ession and use the resulting function.
withSimpleJIT
  :: NFData a
  => Expr
  -> ((Double -> Double) -> a) -- ^ what to do with the generated functiion
  -> IO a
withSimpleJIT expr doFun = LLVMJIT.withContext $ \context -> (>>) (LLVMJIT.loadLibraryPermanently Nothing) $
  LLVMJIT.withModuleFromAST context (codegen expr) $ \mod' ->
  LLVMJIT.withHostTargetMachine $ \tm ->
  LLVMJIT.withObjectLinkingLayer $ \objectLayer ->
  LLVMJIT.withIRCompileLayer objectLayer tm $ \compileLayer -> do
    asm <- LLVMJIT.moduleLLVMAssembly mod'
    printExpr expr
    printIR asm
    LLVMJIT.withModule compileLayer mod' (resolv compileLayer) $ \moduleSet -> do
      fSymbol <- LLVMJIT.mangleSymbol compileLayer "f"
      LLVMJIT.JITSymbol fnAddr _ <- LLVMJIT.findSymbol compileLayer fSymbol True
      let f = mkDoubleFun . castPtrToFunPtr $ wordPtrToPtr fnAddr
      liftIO (putStrLn "*** Result ***\n")
      evaluate $ force (doFun f)

-- * Utilities

cataM
  :: (Monad m, Traversable (Base t), Recursive t)
  => (Base t a -> m a) -> t -> m a
cataM alg = c where
  c = alg <=< traverse c . project

-- * Main

f :: Floating a => a -> a
f t = sin (pi * t / 2) * (1 + sqrt t) ^ 2

main :: IO ()
main = do
  let res1 = map f [0..10] :: [Double]
  res2 <- withSimpleJIT (f x) (\fopt -> map fopt [0..10])
  if res1 == res2
    then putStrLn "results match" >> print res1
    else print res1 >> print res2 >> putStrLn "results don't match"
