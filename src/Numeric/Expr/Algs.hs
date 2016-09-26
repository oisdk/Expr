{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Numeric.Expr.Algs where

import           Numeric.Expr.ExprF
import           Test.QuickCheck

appF :: Floating a => Func -> a -> a
appF = \case
  Exp -> exp; Sin -> sin; Cos -> cos; Tan -> tan; Log -> log
  Atn -> atan; Snh -> sinh; Csh -> cosh; Tnh -> tanh; Asn -> asin
  Acs -> acos; Ach -> acosh; Ash -> asinh; Ath -> atanh

evalAlg :: ExprF n 'NoVar n -> n
evalAlg = \case
  LitF a -> a
  x :+ y -> x + y
  x :- y -> x - y
  x :* y -> x * y
  AbsF x -> abs x
  SigF x -> signum x
  NegF x -> negate x
  x :÷ y -> quot x y
  x :% y -> rem x y
  x :/ y -> x / y
  f :$ x -> appF f x
  x :^ y -> x ** y

safeEvalAlg :: Eq n => ExprF n 'NoVar n -> Maybe n
safeEvalAlg = \case
  _ :/ 0 -> Nothing
  _ :÷ 0 -> Nothing
  _ :% 0 -> Nothing
  x -> Just $ evalAlg x


litArb :: (Num a, Arbitrary a) => r -> [Gen (ExprF a v r)]
litArb = const [LitF . abs <$> arbitrary]

numArb :: Num a => r -> [Gen (ExprF a v r)]
numArb r = map pure
  [ r :+ r, r :- r, r :* r
  , AbsF r, SigF r, NegF r ]

intArb :: Integral a => r -> [Gen (ExprF a v r)]
intArb r = map pure [r :÷ r, r :% r]

fracArb :: Fractional a => r -> [Gen (ExprF a v r)]
fracArb r = [pure (r :/ r)]

floatArb :: Floating a => r -> [Gen (ExprF a v r)]
floatArb r = [pure $ r :^ r, flip (:$) r <$> arbitrary]

varArb :: (Show v, Arbitrary v) => r -> [Gen (ExprF a ('HasVar v) r)]
varArb = const [VarF <$> arbitrary]
