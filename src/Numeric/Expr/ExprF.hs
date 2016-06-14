{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE RankNTypes                 #-}

module Numeric.Expr.ExprF where

import           Control.Lens
import           Data.Coerce
import           Data.Functor.Foldable
import           Data.Functor.Foldable.Extras
import           Data.Serialize
import           GHC.Generics                 (Generic)
import           Test.QuickCheck
import           Data.Function

data Func =
    Sin | Cos | Exp | Log | Tan | Atn | Asn
  | Acs | Snh | Csh | Tnh | Ach | Ash | Ath
          deriving (Eq, Ord, Enum, Bounded, Generic)

instance Arbitrary Func where arbitrary = arbitraryBoundedEnum
instance Serialize Func

instance Show Func where
  show = \case
    Exp -> "exp"; Sin -> "sin"; Cos -> "cos"; Tan -> "tan";
    Log -> "log"; Atn -> "atan"; Snh -> "sinh"; Csh -> "cosh"
    Tnh -> "tanh"; Asn -> "asin"; Acs -> "acos"; Ach -> "acosh"
    Ash -> "asinh"; Ath -> "atanh"

appF :: Floating a => Func -> a -> a
appF = \case
  Exp -> exp; Sin -> sin; Cos -> cos; Tan -> tan; Log -> log
  Atn -> atan; Snh -> sinh; Csh -> cosh; Tnh -> tanh; Asn -> asin
  Acs -> acos; Ach -> acosh; Ash -> asinh; Ath -> atanh

-- | Kind to represent whether or not an expression can hold variables
data VarAbility = HasVar | NoVar deriving (Eq, Ord)

-- | Unfixed expression type
data ExprF littype hasvar vartype r where
  LitF :: n -> ExprF n v t r
  VarF :: t -> ExprF n 'HasVar t r

  -- Num
  (:+) :: Num n => r -> r -> ExprF n v t r
  (:-) :: Num n => r -> r -> ExprF n v t r
  (:*) :: Num n => r -> r -> ExprF n v t r
  AbsF :: Num n => r -> ExprF n v t r
  SigF :: Num n => r -> ExprF n v t r
  NegF :: Num n => r -> ExprF n v t r

  -- Integral
  (:÷) :: Integral n => r -> r -> ExprF n v t r
  (:%) :: Integral n => r -> r -> ExprF n v t r

  -- Fractional
  (:/) :: Fractional n => r -> r -> ExprF n v t r

  -- Floating
  (:$) :: Floating n => Func -> r -> ExprF n v t r
  (:^) :: Floating n => r -> r -> ExprF n v t r

deriving instance Functor (ExprF n hv vt)
deriving instance Foldable (ExprF n hv vt)
deriving instance Traversable (ExprF n hv vt)
deriving instance (Eq a, Eq vt, Eq r) => Eq (ExprF a hv vt r)
deriving instance (Ord a, Ord vt, Ord r) => Ord (ExprF a hv vt r)

evalAlg :: ExprF n 'NoVar () n -> n
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

-- | Fixed

newtype Expr' lit hasvar vartype = Expr'
   { getExpr' :: ExprF lit hasvar vartype (Expr' lit hasvar vartype)
   } deriving (Eq, Ord)

makeWrapped ''Expr'

instance Plated (Expr' lit hasvar vartype) where
  plate = _Wrapped'.traversed
instance ExprType (Expr' lit hasvar vartype) where _Expr = coerced
type instance Base (Expr' lit hasvar vartype) =
  ExprF lit hasvar vartype
instance Recursive (Expr' lit hasvar vartype) where project = getExpr'
instance Corecursive (Expr' lit hasvar vartype) where embed = Expr'

instance Num lit => Num (Expr' lit hv vt) where
  (+) = (:+:)
  (*) = (:*:)
  (-) = (:-:)
  abs = Abs
  signum = Sig
  negate = Neg
  fromInteger = Lit . fromInteger

instance Real a => Real (Expr' a 'NoVar ()) where
  toRational = toRational . cata evalAlg

instance Enum a => Enum (Expr' a 'NoVar ()) where
  toEnum = Lit . toEnum
  fromEnum = fromEnum . cata evalAlg

instance Integral a => Integral (Expr' a 'NoVar ()) where
  toInteger = toInteger . cata evalAlg
  quotRem x y = (x :÷: y, x :%: y)
  quot = (:÷:)
  rem = (:%:)

instance Fractional a => Fractional (Expr' a hv vt) where
  fromRational = Lit . fromRational
  (/) = (:/:)

instance Floating a => Floating (Expr' a hv vt) where
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
  (**) = (:^:)


pprAlg :: (Show a, Show vt) => ExprF a hv vt (Int, ShowS) -> ShowS
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

prec :: ExprF a hv vt r -> Int
prec = \case
  LitF _ -> 11; VarF _ -> 11; AbsF _ -> 10; SigF _ -> 10; _ :$ _ -> 10;
  _ :^ _ -> 8; _ :÷ _ -> 7; _ :% _ -> 7; _ :/ _ -> 7; _ :* _ -> 7
  _ :+ _ -> 6; _ :- _ -> 6; NegF _ -> 0

instance (Show a, Show v) => Show (Expr' a hv v) where
  showsPrec _ = zygo prec pprAlg

litArb :: (Num a, Arbitrary a) => r -> [Gen (ExprF a hv vt r)]
litArb = const [LitF . abs <$> arbitrary]

numArb :: Num a => r -> [Gen (ExprF a hv vt r)]
numArb r = map pure
  [ r :+ r, r :- r, r :* r
  , AbsF r, SigF r, NegF r ]

intArb :: Integral a => r -> [Gen (ExprF a hv vt r)]
intArb r = map pure [r :÷ r, r :% r]

fracArb :: Fractional a => r -> [Gen (ExprF a hv vt r)]
fracArb r = [ pure (r :/ r) ]

floatArb :: Floating a => r -> [Gen (ExprF a hv vt r)]
floatArb r = [ pure $ r :^ r, flip (:$) r <$> arbitrary ]

varArb :: Arbitrary vt => r -> [Gen (ExprF a 'HasVar vt r)]
varArb = const [VarF <$> arbitrary]

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

type family LitType e
type family VarPresent e :: VarAbility
type family VarType e

type instance LitType (Expr a) = a
type instance VarPresent (Expr a) = 'NoVar
type instance VarType (Expr a) = ()

type instance LitType (VarExpr a) = a
type instance VarPresent (VarExpr a) = 'HasVar
type instance VarType (VarExpr a) = String

type instance LitType (Expr' a hv vt) = a
type instance VarPresent (Expr' a hv vt) = hv
type instance VarType (Expr' a hv vt) = vt

class ( Plated e
      , Recursive e
      , Corecursive e
      , Base e ~ ExprF (LitType e) (VarPresent e) (VarType e)
      ) => ExprType e where
  _Expr :: Iso' e (ExprF (LitType e) (VarPresent e) (VarType e) e)


newtype Expr a = Expr
  { getExpr :: Expr' a 'NoVar ()
  } deriving (Eq, Ord, Num, Real, Enum, Integral, Fractional, Floating)

instance Show a => Show (Expr a) where
  show = show . getExpr

instance ExprType (Expr a) where
  _Expr = coerced

instance Plated (Expr a) where plate = _Expr.traversed
type instance Base (Expr a) = ExprF a 'NoVar ()
instance Recursive (Expr a) where project = coerce
instance Corecursive (Expr a) where embed = coerce

newtype VarExpr a = VarExpr
  { getVarExpr :: Expr' a 'HasVar String
  } deriving (Eq, Ord, Num, Fractional, Floating)

instance Show a => Show (VarExpr a) where
  show = show . getVarExpr

instance ExprType (VarExpr a) where _Expr = coerced
instance Plated (VarExpr a) where plate = _Expr.traversed
type instance Base (VarExpr a) = ExprF a 'HasVar String
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
  x :+: (y :+: z) -> Just $ (x :+: y) :+: z
  _               -> Nothing

approxEqual :: ExprType e => (LitType e -> LitType e -> Bool) -> e -> e -> Bool
approxEqual eq = zipo (~=) `on` assoc where
  LitF a ~= LitF b = eq a b
  (w :+ x) ~= (y :+ z) = w y && x z
  (w :- x) ~= (y :- z) = w y && x z
  (w :^ x) ~= (y :^ z) = w y && x z
  (w :* x) ~= (y :* z) = w y && x z
  AbsF x ~= AbsF y = x y
  SigF x ~= SigF y = x y
  NegF x ~= NegF y = x y
  (w :÷ x) ~= (y :÷ z) = w y && x z
  (w :% x) ~= (y :% z) = w y && x z
  (w :/ x) ~= (y :/ z) = w y && x z
  (w :$ x) ~= (y :$ z) = w == y && x z
  _ ~= _ = False

simplify :: (ExprType e, Eq e, Num e) => e -> e
simplify = rewrite $ \case
  x :+: 0 -> Just x
  0 :+: x -> Just x
  x :/: 1 -> Just x
  1 :*: x -> Just x
  x :*: 1 -> Just x
  x :^: 1 -> Just x
  1 :^: _ -> Just $ Lit 1
  _ :^: 0 -> Just $ Lit 1
  0 :*: _ -> Just $ Lit 0
  _ :*: 0 -> Just $ Lit 0
  _ :%: 1 -> Just $ Lit 0
  Neg 0   -> Just $ Lit 0
  x :-: y | x == y -> Just $ Lit 0
  x :/: y | x == y -> Just $ Lit 1
  x :%: y | x == y -> Just $ Lit 0
  x :÷: y | x == y -> Just $ Lit 1
  _ -> Nothing

flatten :: Expr (Expr a) -> Expr a
flatten = cata evalAlg
