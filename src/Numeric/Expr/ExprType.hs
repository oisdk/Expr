{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Numeric.Expr.ExprType
  ( Expr
  , VarExpr
  , HoleExpr
  , IntExpr(..)
  , ExprType(..)
  , LitType
  , VarType
  , pattern Lit
  , pattern Var
  , pattern (:+:)
  , pattern (:*:)
  , pattern (:-:)
  , pattern Abs
  , pattern Neg
  , pattern Sig
  , pattern (:÷:)
  , pattern (:%:)
  , pattern (:/:)
  , pattern (:$:)
  , pattern (:^:)
  , eval
  , assoc
  , approxEqual
  , simplify
  , safeEval
  , getVars
  , repVars
  , varApproxEqual
  ) where

import           Control.Lens                 hiding (elements)
import           Data.Coerce
import           Data.Function
import           Data.Functor.Foldable
import           Data.Functor.Foldable.Extras
import           Data.Monoid
import           Data.Text.ExprPrint          hiding (Lit)
import qualified Data.Text.ExprPrint          as Print
import           GHC.Exts
import           Numeric.Expr.Algs
import           Numeric.Expr.ExprF
import           Test.QuickCheck

newtype Expr' lit var = Expr'
   { _getExpr' :: ExprF lit var (Expr' lit var) }

deriving instance Eq lit => Eq (Expr' lit 'NoVar)
deriving instance (Eq lit, Eq var) => Eq (Expr' lit ('HasVar var))
deriving instance Ord lit => Ord (Expr' lit 'NoVar)
deriving instance (Ord lit, Ord var) => Ord (Expr' lit ('HasVar var))

instance Plated (Expr' lit var) where
  plate f (Expr' e) = Expr' <$> traverse f e
instance ExprType (Expr' lit var) where _Expr = coerced
type instance Base (Expr' lit var) = ExprF lit var
instance Recursive (Expr' lit var) where project = coerce
instance Corecursive (Expr' lit var) where embed = coerce

instance Num lit => Num (Expr' lit var) where
  (+) = (:+:)
  (*) = (:*:)
  (-) = (:-:)
  abs = Abs
  signum = Sig
  negate = Neg
  fromInteger = Lit . fromInteger

instance Real a => Real (Expr' a 'NoVar) where
  toRational = toRational . eval

instance Enum a => Enum (Expr' a 'NoVar) where
  toEnum = Lit . toEnum
  fromEnum = fromEnum . eval

instance Integral a => Integral (Expr' a 'NoVar) where
  toInteger = toInteger . eval
  quotRem x y = (x :÷: y, x :%: y)
  quot = (:÷:)
  rem = (:%:)

instance Fractional a => Fractional (Expr' a v) where
  fromRational = Lit . fromRational
  (/) = (:/:)

instance Floating a => Floating (Expr' a v) where
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

instance (Show a) => Show (Expr' a v) where
  showsPrec _ = appEndo . showExpr (Endo . showParen True . appEndo) (proj . project) . assoc where
    proj = \case
      LitF x -> Print.Lit (ff x)
      VarF x -> Print.Lit (ff x)
      AbsF x -> Prefix (Operator L 10 (fs "abs ")) x
      SigF x -> Prefix (Operator L 10 (fs "signum ")) x
      NegF x -> Prefix (Operator L 10 (fs "negate ")) x
      f :$ x -> Prefix (Operator L 10 (ff f <> fs " ")) x
      x :^ y -> Binary (Operator R 8 (fs " ^ ")) x y
      x :÷ y -> Binary (Operator L 7 (fs " ÷ ")) x y
      x :% y -> Binary (Operator L 7 (fs " % ")) x y
      x :/ y -> Binary (Operator L 7 (fs " / ")) x y
      x :* y -> Binary (Operator L 7 (fs " * ")) x y
      x :+ y -> Binary (Operator L 6 (fs " + ")) x y
      x :- y -> Binary (Operator L 6 (fs " - ")) x y
    fs = Endo . showString
    ff :: Show a => a -> Endo String
    ff = Endo . shows


type family LitType e
type family VarType e :: VarAbility *

type instance LitType (Expr a) = a
type instance VarType (Expr a) = 'NoVar

type instance LitType (HoleExpr v a) = a
type instance VarType (HoleExpr v a) = 'HasVar v

type instance LitType (Expr' a v) = a
type instance VarType (Expr' a v) = v

class ( Plated e
      , Recursive e
      , Corecursive e
      , Base e ~ ExprF (LitType e) (VarType e)
      ) => ExprType e where
  _Expr :: Iso' e (ExprF (LitType e) (VarType e) e)

newtype Expr a = Expr
  { getExpr :: Expr' a 'NoVar
  } deriving ( Eq, Ord, Num, Real, Enum
             , Integral, Fractional, Floating)

instance Show a => Show (Expr a) where
  show = show . getExpr

instance ExprType (Expr a) where
  _Expr = coerced

instance Plated (Expr a) where plate = _Expr.traversed
type instance Base (Expr a) = ExprF a 'NoVar
instance Recursive (Expr a) where project = coerce
instance Corecursive (Expr a) where embed = coerce

newtype HoleExpr v a = HoleExpr
  { getVarExpr :: Expr' a ('HasVar v)
  } deriving (Eq, Ord, Num, Fractional, Floating)

newtype VarString = VarString
  { getVarString :: String
  } deriving (Eq, Ord)

type VarExpr a = HoleExpr VarString a

instance Show VarString where show = getVarString
instance IsString VarString where fromString = VarString
instance (Show v, IsString v) => IsString (HoleExpr v a) where fromString = Var . fromString
instance Arbitrary VarString where
  arbitrary = VarString <$> ((:) <$> elements starts <*> listOf (elements ends)) where
    starts = ['a'..'z'] ++ ['A'..'Z']
    ends = starts ++ ['0'..'9']

instance Show a => Show (VarExpr a) where
  show = show . getVarExpr

instance ExprType (HoleExpr v a) where _Expr = coerced
instance Plated (HoleExpr v a) where plate = _Expr.traversed
type instance Base (HoleExpr v a) = ExprF a ('HasVar v)
instance Recursive (HoleExpr v a) where project = coerce
instance Corecursive (HoleExpr v a) where embed = coerce

pattern x :*: y <- (view _Expr -> x :* y) where
  x :*: y = review _Expr (x :* y)
pattern x :+: y <- (view _Expr -> x :+ y) where
  x :+: y = review _Expr (x :+ y)
pattern x :-: y <- (view _Expr -> x :- y) where
  x :-: y = review _Expr (x :- y)
pattern x :/: y <- (view _Expr -> x :/ y) where
  x :/: y = review _Expr (x :/ y)
pattern x :%: y <- (view _Expr -> x :% y) where
  x :%: y = review _Expr (x :% y)
pattern x :÷: y <- (view _Expr -> x :÷ y) where
  x :÷: y = review _Expr (x :÷ y)
pattern x :^: y <- (view _Expr -> x :^ y) where
  x :^: y = review _Expr (x :^ y)
pattern x :$: y <- (view _Expr -> x :$ y) where
  x :$: y = review _Expr (x :$ y)
pattern Abs x <- (view _Expr -> AbsF x) where
  Abs x = review _Expr (AbsF x)
pattern Sig x <- (view _Expr -> SigF x) where
  Sig x = review _Expr (SigF x)
pattern Var x <- (view _Expr -> VarF x) where
  Var x = review _Expr (VarF x)
pattern Lit x <- (view _Expr -> LitF x) where
  Lit x = review _Expr (LitF x)
pattern Neg x <- (view _Expr -> NegF x) where
  Neg x = review _Expr (NegF x)

assoc :: ExprType e => e -> e
assoc = rewrite $ \case
  x :*: (y :*: z) -> Just $ (x :*: y) :*: z
  x :*: (y :/: z) -> Just $ (x :*: y) :/: z
  x :+: (y :-: z) -> Just $ (x :+: y) :-: z
  x :+: (y :+: z) -> Just $ (x :+: y) :+: z
  _               -> Nothing

-- | Checks for equality after converting associative operators
-- etc into a standard form.
-- >>> let eq = approxEqual (==) :: Expr Int -> Expr Int -> Bool
-- >>> eq ((1 + 2) + 3) (1 + (2 + 3))
-- True
-- >>> let deq = approxEqual (\x y -> abs (x-y) < 0.1) :: Expr Double -> Expr Double -> Bool
-- >>> deq ((1 + 2) + 3) (1 + (2 + 2.99))
-- True
approxEqual :: ExprType e => (LitType e -> LitType e -> Bool) -> e -> e -> Bool
approxEqual eq = zipo (~=) `on` assoc where
  (~=) = zipExpr eq (==) ($) (&&) False

varApproxEqual :: (ExprType e, VarType e ~ 'HasVar v, Eq v)
               => (LitType e -> LitType e -> Bool) -> e -> e -> Bool
varApproxEqual eq = zipo (~=) `on` assoc where
  VarF x ~= VarF y = x == y
  x ~= y = zipExpr eq (==) ($) (&&) False x y

-- | Does some (very basic) simplification
-- >>> :set -XOverloadedStrings
-- >>> simplify (1 + 0 :: Expr Int)
-- 1
--
-- >>> simplify ("x" * 0 :: VarExpr Int)
-- 0

simplify :: (ExprType e, Eq e, Num e) => e -> e
simplify = rewrite $ \case
  x :+: 0 -> Just x
  0 :+: x -> Just x
  x :/: 1 -> Just x
  1 :*: x -> Just x
  x :*: 1 -> Just x
  x :^: 1 -> Just x
  1 :^: _ -> Just 1
  _ :^: 0 -> Just 1
  0 :*: _ -> Just 0
  _ :*: 0 -> Just 0
  _ :%: 1 -> Just 0
  Neg 0   -> Just 0
  x :-: y | x == y -> Just 0
  x :/: y | x == y -> Just 1
  x :%: y | x == y -> Just 0
  x :÷: y | x == y -> Just 1
  _ -> Nothing

eval :: (ExprType e, VarType e ~ 'NoVar) => e -> LitType e
eval = cata evalAlg

-- | Avoids zero - division errors
--
-- >>> safeEval (1 / 0 :: Expr Double)
-- Nothing

safeEval :: (ExprType e, VarType e ~ 'NoVar, Eq (LitType e)) => e -> Maybe (LitType e)
safeEval = cataM safeEvalAlg

instance (Floating a, Arbitrary a) => Arbitrary (Expr a) where
  arbitrary = sized (anaM alg) where
    alg 0 = oneof $ litArb 0
    alg n = oneof $ litArb r ++ floatArb r ++ fracArb r ++ numArb r where
      r = n `div` 2

instance (Floating a, Arbitrary a, Arbitrary v, Show v) => Arbitrary (HoleExpr v a) where
  arbitrary = sized (anaM alg) where
    alg 0 = oneof $ litArb 0 ++ varArb 0
    alg n = oneof $ litArb r ++ floatArb r ++ fracArb r ++ numArb r ++ varArb r where
      r = n `div` 2

newtype IntExpr a = IntExpr
  { getIntExpr :: Expr a
  } deriving (Eq, Ord, Num, Real, Enum, Integral, Show)

instance (Integral a, Arbitrary a) => Arbitrary (IntExpr a) where
  arbitrary = IntExpr <$> sized (anaM alg) where
    alg 0 = oneof $ litArb 0
    alg n = oneof $ litArb r ++ numArb r ++ intArb r where
      r = n `div` 2

-- | Extracts the variables from an expression
--
-- >>> :set -XOverloadedStrings
-- >>> getVars (1 + "a" - 4 - "b" :: VarExpr Int)
-- [a,b]

getVars :: (ExprType e, VarType e ~ 'HasVar a, Show a) => e -> [a]
getVars = toListOf (cosmos._Expr._VarF)

repVars :: (Monad f, ExprType e, VarType e ~ 'HasVar a)
        => (a -> f (Expr (LitType e))) -> e -> f (Expr (LitType e))
repVars f = cataM (either f (pure.embed) . getVar)
