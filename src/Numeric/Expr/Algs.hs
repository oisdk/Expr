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

pprAlg :: (Show a) => ExprF a v (Associativity, ShowS) -> ShowS
pprAlg e = case e of
  LitF a -> shows a
  VarF a -> shows a
  NegF (c,x) -> showString "-" . showParen (11 > precedence c) x
  x :+ y -> bin " + " x y
  x :- y -> bin " - " x y
  x :/ y -> bin " / " x y
  x :* y -> bin " * " x y
  x :^ y -> bin " ^ " x y
  f :$ x -> shows f . showChar ' ' . parR x
  AbsF x -> showString "abs " . parR x
  SigF x -> showString "signum " . parR x
  x :÷ y -> bin " // " x y
  x :% y -> bin " % " x y
  where
    bin s x y = parL x . showString s . parR y
    parL = uncurry $ showParen . isParens LeftS  (prec e)
    parR = uncurry $ showParen . isParens RightS (prec e)
    isParens sid (A ao po) (A ai pi_) =
      pi_ <= po && (pi_ /= po || ai /= ao || ao /= sid)

data Associativity = A
  { side        :: Side
  , precedence  :: Int }

data Side = LeftS | RightS deriving Eq

brcAlg :: (a -> String) -> ExprF a 'NoVar ShowS -> ShowS
brcAlg s = \case
  LitF a -> showString (s a)
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

prec :: ExprF a v r -> Associativity
prec = \case
  LitF _ -> al 11
  VarF _ -> al 11
  AbsF _ -> al 10
  SigF _ -> al 10
  _ :$ _ -> al 10
  _ :^ _ -> A RightS 8
  _ :÷ _ -> al 7
  _ :% _ -> al 7
  _ :/ _ -> al 7
  _ :* _ -> al 7
  _ :+ _ -> al 6
  _ :- _ -> al 6
  NegF _ -> al 0
  where al = A LeftS

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

varArb :: (Show v, Arbitrary v) => r -> [Gen (ExprF a ('HasVar v) r)]
varArb = const [VarF <$> arbitrary]
