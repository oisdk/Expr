{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE FlexibleContexts  #-}

module Numeric.Expr
  ( ExprF(..)
  , Func(..)
  , Expr
  , NumExpr(..)
  , IntExpr(..)
  , FracExpr(..)
  , VarExpr(..)
  , MathML
  , CanVar(..)
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
  , flatten
  , flattenVar
  , appF
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

flatten :: Expr (Expr a) -> Expr a
flatten = cata flatAlg

flattenVar :: VarExpr (VarExpr a) -> VarExpr a
flattenVar = cata $ \case
   VarF s -> Var s
   RecExprF e -> flatAlg e

flatAlg :: ExprF a a -> a
flatAlg = \case
  LitF x   -> x
  AddF x y -> x + y
  SubF x y -> x - y
  MulF x y -> x * y
  AbsF x   -> abs x
  SigF x   -> signum x
  NegF x   -> negate x
  QutF x y -> quot x y
  RemF x y -> rem x y
  DivF x y -> x / y
  AppF f x -> appF f x
  PowF x y -> x ** y

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
assoc :: (Plated a, AsExprF a b a) => a -> a
assoc = rewrite $ \case
    x :+: (y :+: z) -> Just $ (x :+: y) :+: z
    x :*: (y :*: z) -> Just $ (x :*: y) :*: z
    _ -> Nothing

-- | Very basic simplification
simplify :: (Num a, Eq a, AsExprF a b a, Plated a) => a -> a
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

approxEqual :: (Plated e, AsExprF e a e, Recursive e, AsExprF (Base e e) a e
               , AsExprF (Base e (e -> Bool)) a (e -> Bool))
            => (a -> a -> Bool) -> e -> e -> Bool
approxEqual eq = zipo (~=) `on` assoc where
  Lit a ~= Lit b = eq a b
  (w :+: x) ~= (y :+: z) = w y && x z
  (w :-: x) ~= (y :-: z) = w y && x z
  (w :^: x) ~= (y :^: z) = w y && x z
  (w :*: x) ~= (y :*: z) = w y && x z
  Abs x ~= Abs y = x y
  Sig x ~= Sig y = x y
  Neg x ~= Neg y = x y
  (w ://: x) ~= (y ://: z) = w y && x z
  (w :%: x) ~= (y :%: z) = w y && x z
  (w :/: x) ~= (y :/: z) = w y && x z
  (w :$: x) ~= (y :$: z) = w == y && x z
  _ ~= _ = False
