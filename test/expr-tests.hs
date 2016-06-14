{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad
import           Data.Functor
import           Numeric.Expr
import           System.Exit
import           Test.QuickCheck
import qualified Test.QuickCheck.Property as P

prop_Eq :: Expr Double -> Bool
prop_Eq e = e == e

prop_ApproxEq :: Expr Double -> Bool
prop_ApproxEq e = approxEqual (==) e e

prop_Cmp :: Expr Double -> Bool
prop_Cmp e = compare e e == EQ

quickCheckExit :: Testable prop => prop -> IO Result
quickCheckExit = resultExit <=< quickCheckResult where
  resultExit r@ Success{}  = pure r
  resultExit r = exitFailure $> r

failWith :: String -> P.Result
failWith r = P.failed { P.reason = r }

return []
runTests = $forAllProperties quickCheckExit

main = runTests
