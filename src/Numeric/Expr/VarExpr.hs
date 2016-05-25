{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}

module Numeric.Expr.VarExpr
  ( VarExpr(..)
  , varExpr
  ) where

import           Data.Functor.Foldable
import           Data.String
import           Numeric.Expr.Algs
import           Numeric.Expr.ExprF
import           Numeric.Expr.Utils

data VarExpr a =
  Var String |
  RecExpr (ExprF a (VarExpr a))
  deriving (Eq, Ord)

data VarOr a r =
  VarF String |
  RecExprF (ExprF a r)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

type instance Base (VarExpr a) = VarOr a
instance Recursive (VarExpr a) where
  project = \case
    Var s -> VarF s
    RecExpr e -> RecExprF e
instance Corecursive (VarExpr a) where
  embed = \case
    VarF s -> Var s
    RecExprF e -> RecExpr e

varExpr :: (String -> b) -> (ExprF a r -> b) -> VarOr a r -> b
varExpr s _ (VarF v) = s v
varExpr _ f (RecExprF e) = f e

instance Show a => Show (VarExpr a) where
  showsPrec _ = zygo palg alg where
    palg = varExpr (const 11) prec
    alg = varExpr showString pprAlg

instance Num a => Num (VarExpr a) where
  (+)         = RecExpr .: AddF
  (*)         = RecExpr .: MulF
  (-)         = RecExpr .: SubF
  negate      = RecExpr .  NegF
  abs         = RecExpr .  AbsF
  signum      = RecExpr .  SigF
  fromInteger = RecExpr .  LitF . fromInteger

instance Fractional a => Fractional (VarExpr a) where
  fromRational = RecExpr . LitF . fromRational
  (/) = RecExpr .: DivF

instance Floating a => Floating (VarExpr a) where
  pi    = RecExpr .  LitF $ pi
  exp   = RecExpr .  AppF Exp
  log   = RecExpr .  AppF Log
  sin   = RecExpr .  AppF Sin
  cos   = RecExpr .  AppF Cos
  asin  = RecExpr .  AppF Asn
  acos  = RecExpr .  AppF Acs
  atan  = RecExpr .  AppF Atn
  sinh  = RecExpr .  AppF Snh
  cosh  = RecExpr .  AppF Csh
  asinh = RecExpr .  AppF Ash
  acosh = RecExpr .  AppF Ach
  atanh = RecExpr .  AppF Ath
  (**)  = RecExpr .: PowF

instance IsString (VarExpr a) where fromString = Var
