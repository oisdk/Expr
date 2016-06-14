{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}

module Numeric.Expr.ExprF
  ( ExprF(..)
  , appF
  , Func(..)
  , ExprType(..)
  , Expr
  , VarExpr
  , VarAbility(..)
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
  ) where

import           Control.Lens
import           Data.Coerce
import           Data.Functor.Foldable
import           Data.Functor.Foldable.Extras
import           Test.QuickCheck
import           Data.Function

data Func =
    Sin | Cos | Exp | Log | Tan | Atn | Asn
  | Acs | Snh | Csh | Tnh | Ach | Ash | Ath
          deriving (Eq, Ord, Enum, Bounded)

instance Arbitrary Func where arbitrary = arbitraryBoundedEnum

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
data VarAbility a = HasVar a | NoVar deriving (Eq, Ord)

-- | Unfixed expression type
data ExprF littype vartype r where
  LitF :: n -> ExprF n v r
  VarF :: Show t => t -> ExprF n ('HasVar t) r

  -- Num
  (:+) :: Num n => r -> r -> ExprF n v r
  (:-) :: Num n => r -> r -> ExprF n v r
  (:*) :: Num n => r -> r -> ExprF n v r
  AbsF :: Num n => r -> ExprF n v r
  SigF :: Num n => r -> ExprF n v r
  NegF :: Num n => r -> ExprF n v r

  -- Integral
  (:÷) :: Integral n => r -> r -> ExprF n v r
  (:%) :: Integral n => r -> r -> ExprF n v r

  -- Fractional
  (:/) :: Fractional n => r -> r -> ExprF n v r

  -- Floating
  (:$) :: Floating n => Func -> r -> ExprF n v r
  (:^) :: Floating n => r -> r -> ExprF n v r

deriving instance Functor (ExprF n vt)
deriving instance Foldable (ExprF n vt)
deriving instance Traversable (ExprF n vt)
instance (Eq a, Eq r) => Eq (ExprF a 'NoVar r) where
  (==) = zipExpr (==) (==) (==) (&&) False
deriving instance (Eq a, Eq r, Eq v) => Eq (ExprF a ('HasVar v) r)
instance (Ord a, Ord r) => Ord (ExprF a 'NoVar r) where
  compare x y =
    mappend
      (compare (prec x) (prec y))
      (zipExpr compare compare compare mappend undefined x y)
deriving instance (Ord a, Ord r, Ord v) => Ord (ExprF a ('HasVar v) r)

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

-- | Fixed

newtype Expr' lit var = Expr'
   { getExpr' :: ExprF lit var (Expr' lit var)}

deriving instance Eq lit => Eq (Expr' lit 'NoVar)
deriving instance (Eq lit, Eq var) => Eq (Expr' lit ('HasVar var))
deriving instance Ord lit => Ord (Expr' lit 'NoVar)
deriving instance (Ord lit, Ord var) => Ord (Expr' lit ('HasVar var))


instance Plated (Expr' lit var) where
  plate f (Expr' e) = Expr' <$> traverse f e
instance ExprType (Expr' lit var) where _Expr = coerced
type instance Base (Expr' lit var) = ExprF lit var
instance Recursive (Expr' lit var) where project = getExpr'
instance Corecursive (Expr' lit var) where embed = Expr'

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
  (**) = (:^:)

pprAlg :: (Show a) => ExprF a v (Int, ShowS) -> ShowS
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

prec :: ExprF a v r -> Int
prec = \case
  LitF _ -> 11; VarF _ -> 11; AbsF _ -> 10; SigF _ -> 10; _ :$ _ -> 10;
  _ :^ _ -> 8; _ :÷ _ -> 7; _ :% _ -> 7; _ :/ _ -> 7; _ :* _ -> 7
  _ :+ _ -> 6; _ :- _ -> 6; NegF _ -> 0

instance (Show a) => Show (Expr' a v) where
  showsPrec _ = zygo prec pprAlg

type family LitType e
type family VarType e :: VarAbility *

type instance LitType (Expr a) = a
type instance VarType (Expr a) = 'NoVar

type instance LitType (VarExpr a) = a
type instance VarType (VarExpr a) = 'HasVar String

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
  } deriving (Eq, Ord, Num, Real, Enum, Integral, Fractional, Floating)

instance Show a => Show (Expr a) where
  show = show . getExpr

instance ExprType (Expr a) where
  _Expr = coerced

instance Plated (Expr a) where plate = _Expr.traversed
type instance Base (Expr a) = ExprF a 'NoVar
instance Recursive (Expr a) where project = coerce
instance Corecursive (Expr a) where embed = coerce

newtype VarExpr a = VarExpr
  { getVarExpr :: Expr' a ('HasVar String)
  } deriving (Eq, Ord, Num, Fractional, Floating)

instance Show a => Show (VarExpr a) where
  show = show . getVarExpr

instance ExprType (VarExpr a) where _Expr = coerced
instance Plated (VarExpr a) where plate = _Expr.traversed
type instance Base (VarExpr a) = ExprF a ('HasVar String)
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
  (~=) = zipExpr eq (==) ($) (&&) False

-- zipVarExpr :: (lit -> lit -> res)
--            -> (var -> var -> res)
--            -> (Func -> Func -> res)
--            -> (ra -> rb -> res)
--            -> (res -> res -> res)
--            -> res
--            -> ExprF lit ('HasVar var) ra
--            -> ExprF lit ('HasVar var) rb
--            -> res
-- zipVarExpr l v f r c d = (~=) where
--   VarF a ~= VarF b = v a b
--   x ~= y = zipExpr l f r c d x y

zipExpr :: (lit -> lit -> res)
        -> (Func -> Func -> res)
        -> (ra -> rb -> res)
        -> (res -> res -> res)
        -> res
        -> ExprF lit v ra
        -> ExprF lit v rb
        -> res
zipExpr l f r c d = (~=) where
  LitF a ~= LitF b = l a b
  (w :+ x) ~= (y :+ z) = r w y `c` r x z
  (w :- x) ~= (y :- z) = r w y `c` r x z
  (w :^ x) ~= (y :^ z) = r w y `c` r x z
  (w :* x) ~= (y :* z) = r w y `c` r x z
  AbsF x ~= AbsF y = r x y
  SigF x ~= SigF y = r x y
  NegF x ~= NegF y = r x y
  (w :÷ x) ~= (y :÷ z) = r w y `c` r x z
  (w :% x) ~= (y :% z) = r w y `c` r x z
  (w :/ x) ~= (y :/ z) = r w y `c` r x z
  (w :$ x) ~= (y :$ z) = f w y `c` r x z
  _ ~= _ = d

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

eval :: (ExprType e, VarType e ~ 'NoVar) => e -> LitType e
eval = cata evalAlg

litArb :: (Num a, Arbitrary a) => r -> [Gen (ExprF a v r)]
litArb = const [LitF . abs <$> arbitrary]

numArb :: Num a => r -> [Gen (ExprF a v r)]
numArb r = map pure
  [ r :+ r, r :- r, r :* r
  , AbsF r, SigF r, NegF r ]

-- intArb :: Integral a => r -> [Gen (ExprF a v r)]
-- intArb r = map pure [r :÷ r, r :% r]

fracArb :: Fractional a => r -> [Gen (ExprF a v r)]
fracArb r = [ pure (r :/ r) ]

floatArb :: Floating a => r -> [Gen (ExprF a v r)]
floatArb r = [ pure $ r :^ r, flip (:$) r <$> arbitrary ]

varArb :: (Show vt, Arbitrary vt) => r -> [Gen (ExprF a ('HasVar vt) r)]
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
