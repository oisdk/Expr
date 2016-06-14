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

pprAlg :: (Show a) => ExprF a v (Int, ShowS) -> ShowS
pprAlg e = case e of
  LitF a -> shows a
  VarF a -> shows a
  NegF (c,x) -> showString "-" . showParen (11 > c) x
  x :+ y -> parL x . showString " + " . parL y
  x :- y -> parL x . showString " - " . parR y
  x :/ y -> parL x . showString " / " . parR y
  x :* y -> parL x . showString " * " . parL y
  x :^ y -> parR x . showString " ^ " . parL y
  f :$ x -> shows f . showChar ' ' . parR x
  AbsF x -> showString "abs " . parR x
  SigF x -> showString "signum " . parR x
  x :÷ y -> parL x . showString " // " . parR y
  x :% y -> parL x . showString " % "  . parR y
  where
    parL = uncurry $ showParen . (prec e > )
    parR = uncurry $ showParen . (prec e >=)

brcAlg :: (Show a, RealFrac a) => ExprF a 'NoVar ShowS -> ShowS
brcAlg = \case
  LitF a -> shows (floor a :: Integer)
  NegF x -> showString "-" . sp x
  x :+ y -> sp x . showString " + " . sp y
  x :- y -> sp x . showString " - " . sp y
  x :/ y -> sp x . showString " / " . sp y
  x :* y -> sp x . showString " * " . sp y
  x :^ y -> sp x . showString " ^ " . sp y
  f :$ x -> shows f . showChar ' ' . sp x
  AbsF x -> showString "abs " . sp x
  SigF x -> showString "signum " . sp x
  x :÷ y -> sp x . showString " // " . sp y
  x :% y -> sp x . showString " % "  . sp y
  where sp = showParen True

prec :: ExprF a v r -> Int
prec = \case
  LitF _ -> 11; VarF _ -> 11; AbsF _ -> 10; SigF _ -> 10; _ :$ _ -> 10;
  _ :^ _ -> 8; _ :÷ _ -> 7; _ :% _ -> 7; _ :/ _ -> 7; _ :* _ -> 7
  _ :+ _ -> 6; _ :- _ -> 6; NegF _ -> 0

litArb :: (Num a, Arbitrary a) => r -> [Gen (ExprF a v r)]
litArb = const [LitF . abs <$> arbitrary]

numArb :: Num a => r -> [Gen (ExprF a v r)]
numArb r = map pure
  [ r :+ r, r :- r, r :* r
  , AbsF r, SigF r, NegF r ]

intArb :: Integral a => r -> [Gen (ExprF a v r)]
intArb r = map pure [r :÷ r, r :% r]

fracArb :: Fractional a => r -> [Gen (ExprF a v r)]
fracArb r = [ pure (r :/ r) ]

floatArb :: Floating a => r -> [Gen (ExprF a v r)]
floatArb r = [ pure $ r :^ r, flip (:$) r <$> arbitrary ]

varArb :: (Show vt, Arbitrary vt) => r -> [Gen (ExprF a ('HasVar vt) r)]
varArb = const [VarF <$> arbitrary]
