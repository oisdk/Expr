{-# LANGUAGE LambdaCase #-}

module Numeric.Expr.Wrappers
  ( NumExpr(..)
  , IntExpr(..)
  , FracExpr(..)
  ) where

import Test.QuickCheck
import Data.Serialize
import Data.Functor.Foldable.Extras
import Data.Functor.Foldable
import Data.Coerce
import Numeric.Expr.Synonyms
import Numeric.Expr.ExprType
import Numeric.Expr.Algs

-- newtype wrappers for subsets of Expr , similar to QuickCheck's
-- Positive / Small, etc

-- | A subset of Expr, which only supports the operations of the Num
-- typeclass. Has different Arbitrary and Serialize instances to
-- Expr. For instance, to generate expressions with only the Num
-- typeclass operations, you might do this:
--
-- > sample $ do
-- >   NumExpr e <- arbitrary
-- >   pure e
--

newtype NumExpr a =
  NumExpr { getNumExpr :: Expr a
          } deriving (Eq, Ord, Show)

-- | A subset of Expr, which only supports the operations of the
-- Integral typeclass. Has different Arbitrary and Serialize
-- instances to Expr.
newtype IntExpr a =
  IntExpr { getIntExpr :: Expr a
          } deriving (Eq, Ord, Show)

-- | A subset of Expr, which only supports the operations of the
-- Fractional typeclass. Has different Arbitrary and Serialize
-- instances to Expr.
newtype FracExpr a =
  FracExpr { getFracExpr :: Expr a
           } deriving (Eq, Ord, Show)

instance (Num a, Arbitrary a) => Arbitrary (NumExpr a) where
  arbitrary = NumExpr <$> sized (anaM alg) where
    alg 0 = litArb
    alg n = oneof $ litArb : fmap pure (numArb r) where
      r = n `div` 2

instance (Integral a, Arbitrary a) => Arbitrary (IntExpr a) where
  arbitrary = IntExpr <$> sized (anaM alg) where
    alg 0 = litArb
    alg n = oneof $ litArb : fmap pure (numArb r ++ intArb r) where
      r = n `div` 2

instance (Fractional a, Arbitrary a) => Arbitrary (FracExpr a) where
  arbitrary = FracExpr <$> sized (anaM alg) where
    alg 0 = litArb
    alg n = oneof $ litArb : fmap pure (numArb r ++ fracArb r) where
      r = n `div` 2

instance (Num a, Serialize a) => Serialize (NumExpr a) where
  put = cata putAlg . getNumExpr
  get = getn where
    getn = coerce (alg =<< getWord8)
    gete = (coerce :: Get (NumExpr a) -> Get (Expr a)) getn
    alg = \case
      0  -> Lit   <$> get
      1  -> Abs   <$> gete
      2  -> Sig   <$> gete
      6  -> Neg   <$> gete
      8  -> (:*:) <$> gete <*> gete
      9  -> (:+:) <$> gete <*> gete
      10 -> (:-:) <$> gete <*> gete
      _ -> error "corrupted binary"

instance (Integral a, Serialize a) => Serialize (IntExpr a) where
  put = cata putAlg . getIntExpr
  get = getn where
    getn = coerce (alg =<< getWord8)
    gete = (coerce :: Get (IntExpr a) -> Get (Expr a)) getn
    alg = \case
      0  -> Lit    <$> get
      1  -> Abs    <$> gete
      2  -> Sig    <$> gete
      3  -> (://:) <$> gete <*> gete
      4  -> (:%:)  <$> gete <*> gete
      6  -> Neg    <$> gete
      8  -> (:*:)  <$> gete <*> gete
      9  -> (:+:)  <$> gete <*> gete
      10 -> (:-:)  <$> gete <*> gete
      _  -> error "corrupted binary"

instance (Fractional a, Serialize a) => Serialize (FracExpr a) where
  put = cata putAlg . getFracExpr
  get = getn where
    getn = coerce (alg =<< getWord8)
    gete = (coerce :: Get (FracExpr a) -> Get (Expr a)) getn
    alg = \case
      0  -> Lit   <$> get
      1  -> Abs   <$> gete
      2  -> Sig   <$> gete
      6  -> Neg   <$> gete
      7  -> (:/:) <$> gete <*> gete
      8  -> (:*:) <$> gete <*> gete
      9  -> (:+:) <$> gete <*> gete
      10 -> (:-:) <$> gete <*> gete
      _  -> error "corrupted binary"
