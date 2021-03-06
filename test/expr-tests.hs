{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad
import           Data.Functor
import           Numeric.Expr
import           System.Exit
import           Test.QuickCheck
import qualified Test.QuickCheck.Property as P
import           Text.Parser.Combinators
import           Text.Trifecta.Parser
import qualified Text.Trifecta.Result     as T
import           Test.DocTest

prop_ApproxEq :: Expr Double -> Bool
prop_ApproxEq e = approxEqual (==) e e

prop_ApproxEqVar :: VarExpr Double -> Bool
prop_ApproxEqVar e = varApproxEqual (==) e e

prop_Cmp :: Expr Double -> Expr Double -> Bool
prop_Cmp = testOrd

prop_CmpVar :: VarExpr Double -> VarExpr Double -> Bool
prop_CmpVar = testOrd

prop_CmpInt :: IntExpr Integer -> IntExpr Integer -> Bool
prop_CmpInt = testOrd

testOrd :: Ord a => a -> a -> Bool
testOrd x y = x == x && y == y && case compare x y of
  LT -> lt && not eq && not gt
  EQ -> not lt && eq && not gt
  GT -> not lt && not eq && gt
  where
    lt = x <  y
    eq = x == y
    gt = x >  y

prop_Add, prop_Mul, prop_Sub :: IntExpr Integer
                             -> IntExpr Integer -> Bool
prop_Add = testOp (+)
prop_Mul = testOp (*)
prop_Sub = testOp (-)

prop_Abs, prop_Sig, prop_Neg :: IntExpr Integer -> Bool
prop_Abs = testFn abs
prop_Sig = testFn signum
prop_Neg = testFn negate

prop_Parse :: Expr Double -> P.Result
prop_Parse =
  testParse
    exprParse
    show
    show
    (approxEqual (\x y -> abs (x-y) < 0.1))

prop_Parse_Min :: IntExpr Integer -> P.Result
prop_Parse_Min (IntExpr e) =
  testMinBracks
    exprParse
    show
    (approxEqual (==))
    e

prop_ParseVar :: VarExpr Double -> P.Result
prop_ParseVar =
  testParse
    varParse
    show
    show
    (varApproxEqual (\x y -> abs (x-y) < 0.1))

prop_ParseInt :: IntExpr Integer -> P.Result
prop_ParseInt (IntExpr e) =
  testParse
    exprParse
    show
    show
    (approxEqual (==))
    e

roundShow :: Double -> String
roundShow = show . (floor :: Double -> Integer)

testParse :: Parser x
          -> (x -> String)
          -> (x -> String)
          -> (x -> x -> Bool)
          -> x -> P.Result
testParse p s d eq e = case parseString (p<*eof) mempty (s e) of
  T.Success r -> if eq e r then P.succeeded else
    failWith ("\nExpected: " ++ d e ++ "\nReceived: " ++ d r)
  T.Failure x -> failWith (show x)

testMinBracks :: Parser x
              -> (x -> String)
              -> (x -> x -> Bool)
              -> x -> P.Result
testMinBracks p s eq e =
  let shown = s e
      pcount = length . filter ('('==) $ shown
      inds = init [0..pcount]
      prse = parseString (p <* eof) mempty
      norm = prse shown
      rems = [ removeParens (Left i) shown | i <- inds ]
      in case norm of
           T.Success r ->
             foldr f P.succeeded rems where
               f el a = case prse el of
                 T.Success sc ->
                   if eq sc r then failWith (el ++ " should not equal: " ++ shown) else a
                 T.Failure _ -> a
           T.Failure x -> failWith (show x)

removeParens :: Either Int Int -> String -> String
removeParens = flip (foldr f base) where
  base _ = ""
  f '(' a (Left 0) = a (Right 0)
  f '(' a (Left n) = '(' : a (Left (n-1) )
  f '(' a (Right n) = '(' : a (Right (n+1))
  f ')' a (Right 0) = a (Right 1)
  f ')' a (Right n) = ')' : a (Right (n-1))
  f c a e = c : a e

testOp :: (forall n. Integral n => n -> n -> n)
       -> IntExpr Integer -> IntExpr Integer -> Bool
testOp op (IntExpr x) (IntExpr y) =
  safeEval (op x y) == (op <$> safeEval x <*> safeEval y)

testFn :: (forall n. Integral n => n -> n) -> IntExpr Integer -> Bool
testFn op (IntExpr x) = safeEval (op x) == (op <$> safeEval x)

quickCheckExit :: Testable prop => prop -> IO Result
quickCheckExit = resultExit <=< quickCheckResult where
  resultExit r@ Success{}  = pure r
  resultExit r = exitFailure $> r

failWith :: String -> P.Result
failWith r = P.failed { P.reason = r }

return []
runTests :: IO Bool
runTests = $forAllProperties quickCheckExit

main :: IO Bool
main = do
  doctest
    [ "-isrc"
    , "src/Numeric/Expr.hs"
    , "src/Numeric/Expr/ExprType.hs"
    , "src/Numeric/Expr/Algs.hs"
    , "src/Numeric/Expr/MathML.hs"
    , "src/Numeric/Expr/ExprF.hs"
    , "src/Numeric/Expr/Parse.hs"]
  runTests
