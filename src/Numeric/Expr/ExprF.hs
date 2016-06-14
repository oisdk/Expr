{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Numeric.Expr.ExprF
  ( ExprF(..)
  , Func(..)
  , VarAbility(..)
  , zipExpr
  ) where

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

prec :: ExprF l v r -> Int
prec = \case
  LitF _ -> 12; VarF _ -> 11; _ :+ _ -> 10; _ :- _ -> 9; _ :* _ -> 8
  AbsF _ -> 7; SigF _ -> 6; NegF _ -> 5; _ :÷ _ -> 4; _ :% _ -> 3
  _ :/ _ -> 2; _ :$ _ -> 1; _ :^ _ -> 0

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
