{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Numeric.Expr.VarExpr
  ( VarExpr(..)
  , VarOr(..)
  , varExpr
  , CanVar(..)
  ) where

import           Control.Lens
import           Data.Functor.Foldable
import           Data.String
import           Numeric.Expr.Algs
import           Numeric.Expr.ExprF
import           Numeric.Expr.Synonyms
import           Numeric.Expr.Utils

data VarExpr a =
  VarE String |
  RecExpr (ExprF a (VarExpr a))
  deriving (Eq, Ord)

instance Plated (VarExpr a) where
  plate f = \case
    VarE s -> pure $ VarE s
    RecExpr e -> RecExpr <$> traverse f e

data VarOr a r =
  VarF String |
  RecExprF (ExprF a r)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance AsExprF (VarExpr a) a (VarExpr a) where
  exprFPrism = prism' RecExpr $ \case RecExpr e -> Just e; _ -> Nothing

instance AsExprF (VarOr a r) a r where
  exprFPrism = prism' RecExprF $ \case RecExprF e -> Just e; _ -> Nothing

instance CanVar (VarExpr a) where
  _Var = prism' VarE $ \case VarE s -> Just s; _ -> Nothing

instance CanVar (VarOr a r) where
  _Var = prism' VarF $ \case VarF s -> Just s; _ -> Nothing

type instance Base (VarExpr a) = VarOr a
instance Recursive (VarExpr a) where
  project = \case
    VarE s -> VarF s
    RecExpr e -> RecExprF e
instance Corecursive (VarExpr a) where
  embed = \case
    VarF s -> VarE s
    RecExprF e -> RecExpr e

varExpr :: (String -> b) -> (ExprF a r -> b) -> VarOr a r -> b
varExpr s _ (VarF v) = s v
varExpr _ f (RecExprF e) = f e

instance Show a => Show (VarExpr a) where
  showsPrec _ = zygo palg alg where
    palg = varExpr (const 11) prec
    alg = varExpr showString pprAlg

instance Num a => Num (VarExpr a) where
  (+)         = (:+:)
  (*)         = (:*:)
  (-)         = (:-:)
  negate      = Neg
  abs         = Abs
  signum      = Sig
  fromInteger = Lit . fromInteger

instance Fractional a => Fractional (VarExpr a) where
  fromRational = RecExpr . LitF . fromRational
  (/) = RecExpr .: DivF

instance Floating a => Floating (VarExpr a) where
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

instance IsString (VarExpr a) where fromString = Var
