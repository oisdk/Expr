{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Functor
import           Data.Ord
import           Data.Serialize           (Serialize, decode, encode)
import           Expr
import           System.Exit
import           Test.QuickCheck
import qualified Test.QuickCheck.Property as P

prop_Serialize :: NumExpr Int -> P.Result
prop_Serialize = checkSerialize

sameResult :: Eq a => (b -> a) -> (b -> a) -> b -> Bool
sameResult = liftA2 (==)

sameResult2 :: Eq a => (c -> b -> a) -> (c -> b -> a) -> c -> b -> Bool
sameResult2 = liftA2 sameResult

isId :: Eq a => (a -> a) -> a -> Bool
isId = sameResult id

checkSerialize :: (Eq a, Serialize a) => a -> P.Result
checkSerialize a = either failWith (\x -> if x == a then P.succeeded else P.failed) . decode . encode $ a

quickCheckExit :: Testable prop => prop -> IO Result
quickCheckExit = resultExit <=< quickCheckResult where
  resultExit r@ Success{}  = pure r
  resultExit r = exitFailure $> r

failWith :: String -> P.Result
failWith r = P.failed { P.reason = r }

return []
runTests = $forAllProperties quickCheckExit

main = runTests
