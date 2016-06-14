{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad
import           Data.Functor
import           Numeric.Expr
import           System.Exit
import           Test.QuickCheck
import qualified Test.QuickCheck.Property as P
import Text.Trifecta.Parser
import qualified Text.Trifecta.Result as T

prop_Eq :: Expr Double -> Bool
prop_Eq e = e == e

prop_ApproxEq :: Expr Double -> Bool
prop_ApproxEq e = approxEqual (==) e e

prop_Cmp :: Expr Double -> Bool
prop_Cmp e = compare e e == EQ

prop_Ord :: Expr Double -> Expr Double -> Bool
prop_Ord x y = case compare x y of
  LT -> x < y
  EQ -> x == y
  GT -> x > y

prop_Add, prop_Mul, prop_Sub :: IntExpr Integer -> IntExpr Integer -> Bool
prop_Add = testOp (+)
prop_Mul = testOp (*)
prop_Sub = testOp (-)

prop_Abs, prop_Sig, prop_Neg :: IntExpr Integer -> Bool
prop_Abs = testFn abs
prop_Sig = testFn signum
prop_Neg = testFn negate

prop_Parse :: Expr Double -> P.Result
prop_Parse e = case parseString (exprParse) mempty (show e) of
  T.Success r -> if (approxEqual (\x y -> abs (x-y) < 0.1) e r) then P.succeeded else
    failWith ("\nExpected: " ++ showBrack e ++ "\nReceived: " ++ showBrack r)
  T.Failure d -> failWith (show d)

testOp :: (forall n. Integral n => n -> n -> n) -> IntExpr Integer -> IntExpr Integer -> Bool
testOp op (IntExpr x) (IntExpr y) = safeEval (op x y) == (op <$> safeEval x <*> safeEval y)

testFn :: (forall n. Integral n => n -> n) -> IntExpr Integer -> Bool
testFn op (IntExpr x) = safeEval (op x) == (op <$> safeEval x)

quickCheckExit :: Testable prop => prop -> IO Result
quickCheckExit = resultExit <=< quickCheckResult where
  resultExit r@ Success{}  = pure r
  resultExit r = exitFailure $> r

failWith :: String -> P.Result
failWith r = P.failed { P.reason = r }

return []
runTests = $forAllProperties quickCheckExit

main = runTests
