{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Numeric.Expr.ExprF
  ( ExprF(..)
  , Func(..)
  , VarAbility(..)
  , zipExpr
  , _NoVar
  , _VarF
  , _LitF
  , getVar
  ) where

import           Control.Lens
import           Test.QuickCheck

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

-- | Kind to represent whether or not an expression can hold variables
data VarAbility a = HasVar a | NoVar

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
deriving instance (Show a, Show v, Show r) => Show (ExprF a ('HasVar v) r)

prec :: ExprF l v r -> Int
prec = \case
  LitF _ -> 12; VarF _ -> 11; _ :+ _ -> 10; _ :- _ -> 9; _ :* _ -> 8
  AbsF _ -> 7; SigF _ -> 6; NegF _ -> 5; _ :÷ _ -> 4; _ :% _ -> 3
  _ :/ _ -> 2; _ :$ _ -> 1; _ :^ _ -> 0

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

_VarF :: Show b => Prism (ExprF l ('HasVar a) r) (ExprF l ('HasVar b) r) a b
_VarF = prism VarF toVar where
  toVar = either Right (Left . review _NoVar) . getVar

_NoVar :: Prism' (ExprF l ('HasVar v) r) (ExprF l 'NoVar r)
_NoVar = prism' toVar fromVar where
  toVar :: ExprF l 'NoVar r -> ExprF l ('HasVar v) r
  toVar = \case
    LitF x -> LitF x
    x :+ y -> x :+ y
    x :- y -> x :- y
    x :* y -> x :* y
    AbsF x -> AbsF x
    SigF x -> SigF x
    NegF x -> NegF x
    x :÷ y -> x :÷ y
    x :% y -> x :% y
    x :/ y -> x :/ y
    x :$ y -> x :$ y
    x :^ y -> x :^ y
  fromVar = either (const Nothing) Just . getVar

getVar :: ExprF l ('HasVar v) r -> Either v (ExprF l 'NoVar r)
getVar = \case
  VarF x -> Left x
  LitF x -> Right (LitF x)
  x :+ y -> Right (x :+ y)
  x :- y -> Right (x :- y)
  x :* y -> Right (x :* y)
  AbsF x -> Right (AbsF x)
  SigF x -> Right (SigF x)
  NegF x -> Right (NegF x)
  x :÷ y -> Right (x :÷ y)
  x :% y -> Right (x :% y)
  x :/ y -> Right (x :/ y)
  x :$ y -> Right (x :$ y)
  x :^ y -> Right (x :^ y)

_LitF :: Prism' (ExprF l v r) l
_LitF = prism' LitF $ \case LitF x -> Just x; _ -> Nothing
