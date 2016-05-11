{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module Expr where

import           Control.Lens
import           Data.Coerce
import           Data.Functor.Foldable hiding (Foldable)
import qualified Data.Functor.Foldable as Functor
import           Data.Monoid
import           Data.Ord
import           GHC.Generics          (Generic)


data ExprF a r where
  Lit :: a -> ExprF a r

  -- Num
  (:+:) :: Num a => r -> r -> ExprF a r
  (:*:) :: Num a => r -> r -> ExprF a r
  Abs   :: Num a => r -> ExprF a r
  Sig   :: Num a => r -> ExprF a r
  Neg   :: Num a => r -> ExprF a r

  -- Integral:
  Qut   :: Integral a => r -> r -> ExprF a r
  Rem   :: Integral a => r -> r -> ExprF a r

  -- Fractional:
  (:/:) :: Fractional a => r -> r -> ExprF a r

  -- Floating:
  (:$:) :: Floating a => Func -> r -> ExprF a r

data Func =
    Sin | Cos | Exp | Log | Tan | Atn | Asn
  | Acs | Snh | Csh | Tnh | Ach | Ash | Ath
          deriving (Eq, Ord, Enum, Bounded, Generic, Show)

instance Functor (ExprF a) where
  fmap f = \case
    Lit a   -> Lit a
    x :+: y -> f x :+: f y
    x :*: y -> f x :+: f y
    Abs x   -> Abs (f x)
    Sig x   -> Sig (f x)
    Neg x   -> Neg (f x)
    Qut x y -> Qut (f x) (f y)
    Rem x y -> Rem (f x) (f y)
    x :/: y -> f x :/: f y
    g :$: x -> g :$: f x

instance Foldable (ExprF a) where
  foldr f i = \case
    Lit _   -> i
    x :+: y -> f x (f y i)
    x :*: y -> f x (f y i)
    Abs x   -> f x i
    Sig x   -> f x i
    Neg x   -> f x i
    Qut x y -> f x (f y i)
    Rem x y -> f x (f y i)
    x :/: y -> f x (f y i)
    _ :$: x -> f x i
  foldMap f = \case
    Lit _   -> mempty
    x :+: y -> f x <> f y
    x :*: y -> f x <> f y
    Abs x   -> f x
    Sig x   -> f x
    Neg x   -> f x
    Qut x y -> f x <> f y
    Rem x y -> f x <> f y
    x :/: y -> f x <> f y
    _ :$: x -> f x

instance Traversable (ExprF a) where
  traverse f = \case
    Lit a   -> pure (Lit a)
    x :+: y -> (:+:) <$> f x <*> f y
    x :*: y -> (:*:) <$> f x <*> f y
    Abs x   -> Abs <$> f x
    Sig x   -> Sig <$> f x
    Neg x   -> Neg <$> f x
    Qut x y -> Qut <$> f x <*> f y
    Rem x y -> Rem <$> f x <*> f y
    x :/: y -> (:/:) <$> f x <*> f y
    g :$: x -> (:$:) g <$> f x

instance (Eq a, Eq r) => Eq (ExprF a r) where
  (==) = zipExpr (\_ _ -> False) (==) (==) (==) (&&)

instance (Ord a, Ord r) => Ord (ExprF a r) where
  compare = zipExpr (comparing prec) compare compare compare mappend

prec :: ExprF a r -> Int
prec = \case
    Lit _   -> 9
    Abs _   -> 8
    Sig _   -> 7
    Qut _ _ -> 6
    Rem _ _ -> 5
    _ :$: _ -> 4
    Neg _   -> 3
    _ :/: _ -> 2
    _ :*: _ -> 1
    _ :+: _ -> 0

zipExpr :: (ExprF a r -> ExprF a r -> b)
        -> (a -> a -> b)
        -> (r -> r -> b)
        -> (Func -> Func -> b)
        -> (b -> b -> b)
        -> ExprF a r
        -> ExprF a r
        -> b
zipExpr d n r f c = i where
  i (Lit a  ) (Lit b  ) = n a b
  i (w :+: x) (y :+: z) = r w y `c` r x z
  i (w :*: x) (y :*: z) = r w y `c` r x z
  i (Abs x  ) (Abs y  ) = r x y
  i (Sig x  ) (Sig y  ) = r x y
  i (Neg x  ) (Neg y  ) = r x y
  i (Qut w x) (Qut y z) = r w y `c` r x z
  i (Rem w x) (Rem y z) = r w y `c` r x z
  i (w :/: x) (y :/: z) = r w y `c` r x z
  i (w :$: x) (y :$: z) = f w y `c` r x z
  i x y = d x y

evalAlg :: ExprF a a -> a
evalAlg = \case
    Lit a   -> a
    x :+: y -> x + y
    x :*: y -> x * y
    Abs x   -> abs x
    Sig x   -> signum x
    Neg x   -> negate x
    Qut x y -> quot x y
    Rem x y -> rem x y
    x :/: y -> x / y
    f :$: x -> appF f x

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

newtype NumExpr a =
  NumExpr { getNumExpr :: Fix (ExprF a)
          } deriving (Eq, Ord)

coerceNumBi :: (Fix (ExprF a) -> Fix (ExprF a) -> ExprF a (Fix (ExprF a))) -> NumExpr a -> NumExpr a -> NumExpr a
coerceNumBi = coerce

coerceNumUn :: (Fix (ExprF a) -> ExprF a (Fix (ExprF a))) -> NumExpr a -> NumExpr a
coerceNumUn = coerce

instance Num a => Num (NumExpr a) where
  (+) = coerceNumBi (:+:)
  (*) = coerceNumBi (:*:)
  abs = coerceNumUn Abs
  signum = coerceNumUn Sig
  negate = coerceNumUn Neg
  fromInteger = NumExpr . Fix . Lit . fromInteger

newtype FracExpr a =
  FracExpr { getFracExpr :: Fix (ExprF a)
           } deriving (Eq, Ord)

coerceFracBi :: (Fix (ExprF a) -> Fix (ExprF a) -> ExprF a (Fix (ExprF a))) -> FracExpr a -> FracExpr a -> FracExpr a
coerceFracBi = coerce

coerceFracUn :: (Fix (ExprF a) -> ExprF a (Fix (ExprF a))) -> FracExpr a -> FracExpr a
coerceFracUn = coerce

instance Num a => Num (FracExpr a) where
  (+) = coerceFracBi (:+:)
  (*) = coerceFracBi (:*:)
  abs = coerceFracUn Abs
  signum = coerceFracUn Sig
  negate = coerceFracUn Neg
  fromInteger = FracExpr . Fix . Lit . fromInteger

instance Fractional a => Fractional (FracExpr a) where
  fromRational = FracExpr . Fix . Lit . fromRational
  (/) = coerceFracBi (:/:)

newtype FloatExpr a =
  FloatExpr { getFloatExpr :: Fix (ExprF a)
            } deriving (Eq, Ord)

