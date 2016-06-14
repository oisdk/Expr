{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE PatternSynonyms            #-}

module Numeric.Expr.ExprType
  ( Expr(..)
  ) where

import           Control.Lens                 (Plated, Wrapped (..), iso, plate)
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

pattern Lit x    = Expr (LitF x)
pattern x :+: y  = Expr (x :+ y)
pattern x :*: y  = Expr (x :* y)
pattern x :-: y  = Expr (x :- y)
pattern Abs x    = Expr (AbsF x)
pattern Sig x    = Expr (SigF x)
pattern Neg x    = Expr (NegF x)
pattern x ://: y = Expr (x :// y)
pattern x :%: y  = Expr (x :% y)
pattern x :/: y  = Expr (x :/ y)
pattern f :$: x  = Expr (f :$ x)
pattern x :^: y  = Expr (x :^ y)

instance Wrapped (Expr a) where
  type Unwrapped (Expr a) = ExprF a (Expr a)
  _Wrapped' = coerced

instance Num a => Num (Expr a) where
  (+) = (:+:)
  (*) = (:*:)
  (-) = (:-:)
  abs = Abs
  signum = Sig
  negate = Neg
  fromInteger = Lit . fromInteger

instance Real a => Real (Expr a) where
  toRational = toRational . cata evalAlg

instance Enum a => Enum (Expr a) where
  toEnum = Lit . toEnum
  fromEnum = fromEnum . cata evalAlg

instance Integral a => Integral (Expr a) where
  toInteger = toInteger . cata evalAlg
  quotRem a b = (a ://: b, a :%: b)
  quot = (://:)
  rem  = (:%:)

instance Fractional a => Fractional (Expr a) where
  fromRational = Lit . fromRational
  (/) = (:/:)

instance Floating a => Floating (Expr a) where
  pi    = Lit pi
  exp   = (:$:) Exp
  log   = (:$:) Log
  sin   = (:$:) Sin
  cos   = (:$:) Cos
  asin  = (:$:) Asn
  acos  = (:$:) Acs
  atan  = (:$:) Atn
  sinh  = (:$:) Snh
  cosh  = (:$:) Csh
  asinh = (:$:) Ash
  acosh = (:$:) Ach
  atanh = (:$:) Ath
  (**)  = (:^:)

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
