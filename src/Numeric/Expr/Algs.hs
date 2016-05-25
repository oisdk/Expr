{-# LANGUAGE LambdaCase #-}

module Numeric.Expr.Algs
  ( evalAlg
  , litArb
  , numArb
  , fracArb
  , intArb
  , floatArb
  , putAlg
  , pprAlg
  , prec
  ) where

import           Data.Serialize
import           Numeric.Expr.ExprF
import           Test.QuickCheck

evalAlg :: ExprF a a -> a
evalAlg = \case
  LitF a   -> a
  AddF x y -> x + y
  SubF x y -> x - y
  PowF x y -> x ** y
  MulF x y -> x * y
  AbsF x   -> abs x
  SigF x   -> signum x
  NegF x   -> negate x
  QutF x y -> quot x y
  RemF x y -> rem x y
  DivF x y -> x / y
  AppF f x -> appF f x


litArb :: (Num a, Arbitrary a) => Gen (ExprF a r)
litArb = LitF . abs <$> arbitrary

numArb :: Num a => r -> [ExprF a r]
numArb r =
  [ AddF r r
  , SubF r r
  , MulF r r
  , AbsF r
  , SigF r
  , NegF r ]

intArb :: Integral a => r -> [ExprF a r]
intArb r =
  [ QutF r r
  , RemF r r ]

fracArb :: Fractional a => r -> [ExprF a r]
fracArb r = [ DivF r r ]

floatArb :: Floating a => r -> [Gen (ExprF a r)]
floatArb r = [ pure $ PowF r r, flip AppF r <$> arbitrary ]


putAlg :: Serialize a => ExprF a (PutM ()) -> PutM ()
putAlg = \case
  LitF a   -> putWord8 0  *> put a
  AbsF x   -> putWord8 1  *> x
  SigF x   -> putWord8 2  *> x
  QutF x y -> putWord8 3  *> x *> y
  RemF x y -> putWord8 4  *> x *> y
  AppF f x -> putWord8 5  *> put f *> x
  NegF x   -> putWord8 6  *> x
  DivF x y -> putWord8 7  *> x *> y
  MulF x y -> putWord8 8  *> x *> y
  AddF x y -> putWord8 9  *> x *> y
  SubF x y -> putWord8 10 *> x *> y
  PowF x y -> putWord8 11 *> x *> y

pprAlg :: Show a => ExprF a (Int, ShowS) -> ShowS
pprAlg e = case e of
  LitF a -> shows a
  NegF (c,x) -> showString "-" . showParen (11 > c) x
  AddF x y -> parL x . showString " + " . parL y
  SubF x y -> parL x . showString " - " . parR y
  DivF x y -> parL x . showString " / " . parR y
  MulF x y -> parL x . showString " * " . parL y
  PowF x y -> parR x . showString " ^ " . parL y
  AppF f x -> shows f . showChar ' ' . parR x
  AbsF x   -> showString "abs " . parR x
  SigF x   -> showString "signum " . parR x
  QutF x y -> parL x . showString " // " . parR y
  RemF x y -> parL x . showString " % "  . parR y
  where
    parL = uncurry $ showParen . (prec e > )
    parR = uncurry $ showParen . (prec e >=)

prec :: ExprF a r -> Int
prec = \case
  LitF _   -> 11
  AbsF _   -> 10
  SigF _   -> 10
  AppF _ _ -> 10
  PowF _ _ -> 8
  QutF _ _ -> 7
  RemF _ _ -> 7
  DivF _ _ -> 7
  MulF _ _ -> 7
  AddF _ _ -> 6
  SubF _ _ -> 6
  NegF _   -> 0
