{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}

module Numeric.Expr.VarExpr
  ( VarExpr
  , VarOr
  , varExpr
  ) where

import           Data.Coerce
import           Data.Functor.Foldable
import           Data.String
import           Numeric.Expr.Algs
import           Numeric.Expr.ExprF
import           Numeric.Expr.Utils

newtype VarExpr a = VarExpr
  { _getVarExpr :: VarOr a (VarExpr a)
  } deriving (Eq, Ord)

data VarOr a r =
  Var String |
  RecExpr (ExprF a r)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

type instance Base (VarExpr a) = VarOr a
instance Recursive (VarExpr a) where project = coerce
instance Corecursive (VarExpr a) where embed = coerce

varExpr :: (String -> b) -> (ExprF a r -> b) -> VarOr a r -> b
varExpr s _ (Var v) = s v
varExpr _ f (RecExpr e) = f e

instance Show a => Show (VarExpr a) where
  showsPrec _ = zygo palg alg where
    palg = varExpr (const 11) prec
    alg = varExpr showString pprAlg

wrapE :: ExprF a (VarExpr a) -> VarExpr a
wrapE = coerce' RecExpr where
  coerce' :: (ExprF a (VarExpr a) -> VarOr a (VarExpr a))
          -> ExprF a (VarExpr a) -> VarExpr a
  coerce' = coerce

instance Num a => Num (VarExpr a) where
  (+)         = wrapE .: AddF
  (*)         = wrapE .: MulF
  (-)         = wrapE .: SubF
  negate      = wrapE .  NegF
  abs         = wrapE .  AbsF
  signum      = wrapE .  SigF
  fromInteger = wrapE .  LitF . fromInteger

instance Fractional a => Fractional (VarExpr a) where
  fromRational = wrapE . LitF . fromRational
  (/) = wrapE .: DivF

instance Floating a => Floating (VarExpr a) where
  pi    = wrapE .  LitF $ pi
  exp   = wrapE .  AppF Exp
  log   = wrapE .  AppF Log
  sin   = wrapE .  AppF Sin
  cos   = wrapE .  AppF Cos
  asin  = wrapE .  AppF Asn
  acos  = wrapE .  AppF Acs
  atan  = wrapE .  AppF Atn
  sinh  = wrapE .  AppF Snh
  cosh  = wrapE .  AppF Csh
  asinh = wrapE .  AppF Ash
  acosh = wrapE .  AppF Ach
  atanh = wrapE .  AppF Ath
  (**)  = wrapE .: PowF

instance IsString (VarExpr a) where fromString = VarExpr . Var
