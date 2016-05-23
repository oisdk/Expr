{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.Expr.ExprType
  ( Expr(..)
  ) where

import           Control.Lens                 (Plated, plate)
import           Data.Coerce
import           Data.Functor.Foldable
import           Data.Functor.Foldable.Extras
import           Data.Serialize
import           Numeric.Expr.Algs
import           Numeric.Expr.ExprF
import           Numeric.Expr.Utils
import           Test.QuickCheck

-- | A fixed expression type, which can conform to the numeric
-- typeclasses depending on what constant type it wraps.
newtype Expr a =
  Expr { _getExpr :: ExprF a (Expr a)
       } deriving (Eq, Ord)

coerceBi :: (Expr a -> Expr a -> ExprF a (Expr a))
         -> Expr a -> Expr a -> Expr a
coerceBi = coerce

instance Num a => Num (Expr a) where
  (+) = coerceBi AddF
  (*) = coerceBi MulF
  (-) = coerceBi SubF
  abs = Expr .# AbsF
  signum = Expr .# SigF
  negate = Expr .# NegF
  fromInteger = Expr .# LitF . fromInteger

instance Real a => Real (Expr a) where
  toRational = toRational . cata evalAlg

instance Enum a => Enum (Expr a) where
  toEnum = Expr .# LitF . toEnum
  fromEnum = fromEnum . cata evalAlg

instance Integral a => Integral (Expr a) where
  toInteger = toInteger . cata evalAlg
  quotRem a b = (coerceBi QutF a b, coerceBi RemF a b)
  quot = coerceBi QutF
  rem  = coerceBi RemF

instance Fractional a => Fractional (Expr a) where
  fromRational = Expr .# LitF . fromRational
  (/) = coerceBi DivF

instance Floating a => Floating (Expr a) where
  pi    = Expr .# LitF $ pi
  exp   = Expr .# AppF Exp
  log   = Expr .# AppF Log
  sin   = Expr .# AppF Sin
  cos   = Expr .# AppF Cos
  asin  = Expr .# AppF Asn
  acos  = Expr .# AppF Acs
  atan  = Expr .# AppF Atn
  sinh  = Expr .# AppF Snh
  cosh  = Expr .# AppF Csh
  asinh = Expr .# AppF Ash
  acosh = Expr .# AppF Ach
  atanh = Expr .# AppF Ath
  (**) = coerceBi PowF

type instance Base (Expr a) = ExprF a
instance Recursive (Expr a) where project = coerce
instance Corecursive (Expr a) where embed = coerce
instance Plated (Expr a) where
  plate f = fmap embed . traverse f . project

instance (Floating a, Arbitrary a) => Arbitrary (Expr a) where
  arbitrary = sized (anaM alg) where
    alg 0 = litArb
    alg n = oneof $
      litArb :
      floatArb r ++ fmap pure (
      numArb r ++
      fracArb r
      ) where r = n `div` 2

instance (Floating a, Serialize a) => Serialize (Expr a) where
  put = cata putAlg
  get = Expr <$> (alg =<< getWord8) where
    alg = \case
      0  -> LitF <$> get
      1  -> AbsF <$> get
      2  -> SigF <$> get
      5  -> AppF <$> get <*> get
      6  -> NegF <$> get
      7  -> DivF <$> get <*> get
      8  -> MulF <$> get <*> get
      9  -> AddF <$> get <*> get
      10 -> SubF <$> get <*> get
      11 -> PowF <$> get <*> get
      _ -> error "corrupted binary"

instance Show a => Show (Expr a) where showsPrec _ = zygo prec pprAlg
