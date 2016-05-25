{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Numeric.Expr.ExprF
  ( ExprF(..)
  , Func(..)
  , appF
  ) where

import           Data.Serialize
import           GHC.Generics
import           Test.QuickCheck

data ExprF a r where
  LitF :: a -> ExprF a r

  -- Num
  AddF :: Num a => r -> r -> ExprF a r
  SubF :: Num a => r -> r -> ExprF a r
  MulF :: Num a => r -> r -> ExprF a r
  AbsF :: Num a => r -> ExprF a r
  SigF :: Num a => r -> ExprF a r
  NegF :: Num a => r -> ExprF a r

  -- Integral
  QutF :: Integral a => r -> r -> ExprF a r
  RemF :: Integral a => r -> r -> ExprF a r

  -- Fractional
  DivF :: Fractional a => r -> r -> ExprF a r

  -- Floating
  AppF :: Floating a => Func -> r -> ExprF a r
  PowF :: Floating a => r -> r -> ExprF a r

deriving instance Functor (ExprF a)
deriving instance Foldable (ExprF a)
deriving instance Traversable (ExprF a)

data Func =
    Sin | Cos | Exp | Log | Tan | Atn | Asn
  | Acs | Snh | Csh | Tnh | Ach | Ash | Ath
          deriving (Eq, Ord, Enum, Bounded, Generic)

instance Arbitrary Func where arbitrary = arbitraryBoundedEnum
instance Serialize Func

instance Show Func where
  show = \case
    Exp -> "exp"
    Sin -> "sin"
    Cos -> "cos"
    Tan -> "tan"
    Log -> "log"
    Atn -> "atan"
    Snh -> "sinh"
    Csh -> "cosh"
    Tnh -> "tanh"
    Asn -> "asin"
    Acs -> "acos"
    Ach -> "acosh"
    Ash -> "asinh"
    Ath -> "atanh"

deriving instance (Eq a, Eq r) => Eq (ExprF a r)
deriving instance (Ord a, Ord r) => Ord (ExprF a r)

appF :: Floating a => Func -> a -> a
appF = \case
  Exp -> exp
  Sin -> sin
  Cos -> cos
  Tan -> tan
  Log -> log
  Atn -> atan
  Snh -> sinh
  Csh -> cosh
  Tnh -> tanh
  Asn -> asin
  Acs -> acos
  Ach -> acosh
  Ash -> asinh
  Ath -> atanh
