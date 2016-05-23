{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Numeric.Expr
  ( ExprF(..)
  , Func(..)
  , Expr
  , NumExpr(..)
  , IntExpr(..)
  , FracExpr(..)
  , VarExpr(..)
  , MathML
  , pattern (:*:)
  , pattern (:+:)
  , pattern (:-:)
  , pattern (:%:)
  , pattern (:/:)
  , pattern (:$:)
  , pattern (:^:)
  , pattern (://:)
  , pattern Neg
  , pattern Abs
  , pattern Lit
  , assoc
  , simplify
  , eval
  , safeEval
  , approxEqual
  , showBracks
  , mlRep
  ) where

import           Control.Lens                 hiding (para)
import           Data.Function
import           Data.Functor.Foldable
import           Data.Functor.Foldable.Extras
import           Numeric.Expr.Algs
import           Numeric.Expr.ExprF
import           Numeric.Expr.ExprType
import           Numeric.Expr.MathML
import           Numeric.Expr.Synonyms
import           Numeric.Expr.VarExpr
import           Numeric.Expr.Wrappers

eval :: Expr a -> a
eval = cata evalAlg

safeEvalAlg :: Eq a => ExprF a a -> Either String a
safeEvalAlg = \case
  DivF _ 0 -> Left "tried to divide by zero"
  e -> Right (evalAlg e)

-- | Evaluate an expression, catching zero-division errors.
safeEval :: Eq a => Expr a -> Either String a
safeEval = cataM safeEvalAlg



showBracks :: Show a => Expr a -> String
showBracks = cata $ \case
  LitF a   -> show a
  NegF x   -> '-' : p x
  AddF x y -> p x ++ " + " ++ p y
  SubF x y -> p x ++ " - " ++ p y
  DivF x y -> p x ++ " / " ++ p y
  MulF x y -> p x ++ " * " ++ p y
  AppF f x -> show f ++ " " ++ p x
  AbsF x   -> "abs " ++ p x
  SigF x   -> "signum " ++ p x
  QutF x y -> p x ++ " // " ++ p y
  PowF x y -> p x ++ " ^ " ++ p y
  RemF x y -> p x ++ " % " ++ p y
  where p s = "(" ++ s ++ ")"


-- | Normalizes associative operators
assoc :: Expr a -> Expr a
assoc = rewrite $ \case
    x :+: (y :+: z) -> Just $ (x :+: y) :+: z
    x :*: (y :*: z) -> Just $ (x :*: y) :*: z
    _ -> Nothing

-- | Very basic simplification
simplify :: (Num a, Eq a) => Expr a -> Expr a
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
  x :-:  y | x == y -> Just $ Lit 0
  x :/:  y | x == y -> Just $ Lit 1
  x :%:  y | x == y -> Just $ Lit 0
  x ://: y | x == y -> Just $ Lit 1
  _ -> Nothing

approxEqual :: (a -> a -> Bool) -> Expr a -> Expr a -> Bool
approxEqual eq = zipo alg `on` assoc where
  alg (LitF a  ) (LitF b  ) = eq a b
  alg (AddF w x) (AddF y z) = w y && x z
  alg (SubF w x) (SubF y z) = w y && x z
  alg (PowF w x) (PowF y z) = w y && x z
  alg (MulF w x) (MulF y z) = w y && x z
  alg (AbsF x  ) (AbsF y  ) = x y
  alg (SigF x  ) (SigF y  ) = x y
  alg (NegF x  ) (NegF y  ) = x y
  alg (QutF w x) (QutF y z) = w y && x z
  alg (RemF w x) (RemF y z) = w y && x z
  alg (DivF w x) (DivF y z) = w y && x z
  alg (AppF w x) (AppF y z) = w == y && x z
  alg _ _ = False






