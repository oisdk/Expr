{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad
import           Data.Functor
import           Data.Serialize           (Serialize, decode, encode)
import           Numeric.Expr
import           System.Exit
import           Test.QuickCheck
import qualified Test.QuickCheck.Property as P

prop_Serialize :: Expr Double -> P.Result
prop_Serialize = checkSerialize

prop_SerializeNum :: NumExpr Integer -> P.Result
prop_SerializeNum = checkSerialize

prop_SerializeInt :: IntExpr Integer -> P.Result
prop_SerializeInt = checkSerialize

prop_SerializeFrac :: FracExpr Rational -> P.Result
prop_SerializeFrac = checkSerialize

prop_Eq :: Expr Double -> Bool
prop_Eq e = e == e

prop_Cmp :: Expr Double -> Bool
prop_Cmp e = compare e e == EQ

checkSerialize :: (Eq a, Serialize a) => a -> P.Result
checkSerialize a =
  either failWith (\x -> if x == a then P.succeeded else P.failed) . decode . encode $ a

quickCheckExit :: Testable prop => prop -> IO Result
quickCheckExit = resultExit <=< quickCheckResult where
  resultExit r@ Success{}  = pure r
  resultExit r = exitFailure $> r

failWith :: String -> P.Result
failWith r = P.failed { P.reason = r }

return []
runTests = $forAllProperties quickCheckExit

main = runTests
