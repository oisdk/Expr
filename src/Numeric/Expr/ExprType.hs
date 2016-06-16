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
  , showBrack
  , showBrackVar
  , varApproxEqual
  ) where

import           Control.Lens                 hiding (elements)
import           Data.Coerce
import           Data.Function
import           Data.Functor.Foldable
import           Data.Functor.Foldable.Extras
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
  showsPrec _ = zygo prec pprAlg . assoc

type family LitType e
type family VarType e :: VarAbility *

type instance LitType (Expr a) = a
type instance VarType (Expr a) = 'NoVar

type instance LitType (VarExpr a) = a
type instance VarType (VarExpr a) = 'HasVar VarString

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

newtype VarExpr a = VarExpr
  { getVarExpr :: Expr' a ('HasVar VarString)
  } deriving (Eq, Ord, Num, Fractional, Floating)

newtype VarString = VarString
  { getVarString :: String
  } deriving (Eq, Ord)

instance Show VarString where show = getVarString
instance IsString VarString where fromString = VarString
instance IsString (VarExpr a) where fromString = Var . fromString
instance Arbitrary VarString where
  arbitrary = VarString <$> ((:) <$> elements starts <*> listOf (elements ends)) where
    starts = ['a'..'z'] ++ ['A'..'Z']
    ends = starts ++ ['0'..'9']

instance Show a => Show (VarExpr a) where
  show = show . getVarExpr

instance ExprType (VarExpr a) where _Expr = coerced
instance Plated (VarExpr a) where plate = _Expr.traversed
type instance Base (VarExpr a) = ExprF a ('HasVar VarString)
instance Recursive (VarExpr a) where project = coerce
instance Corecursive (VarExpr a) where embed = coerce

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
  1 :^: _ -> Just (Lit 1)
  _ :^: 0 -> Just (Lit 1)
  0 :*: _ -> Just (Lit 0)
  _ :*: 0 -> Just (Lit 0)
  _ :%: 1 -> Just (Lit 0)
  Neg 0   -> Just (Lit 0)
  x :-: y | x == y -> Just (Lit 0)
  x :/: y | x == y -> Just (Lit 1)
  x :%: y | x == y -> Just (Lit 0)
  x :÷: y | x == y -> Just (Lit 1)
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

instance (Floating a, Arbitrary a) => Arbitrary (VarExpr a) where
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

showBrack :: (a -> String) -> Expr a -> String
showBrack s e = cata (brcAlg s) e ""

showBrackVar :: (a -> String) -> VarExpr a -> String
showBrackVar s e = cata alg e "" where
  alg = either (shows.show) (brcAlg s) . getVar
