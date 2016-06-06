{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE ViewPatterns           #-}

module Numeric.Expr.Synonyms where

import           Control.Lens
import           Numeric.Expr.ExprF
import           Numeric.Expr.ExprType

class AsExprF a b c | a -> b, a -> c where exprFPrism :: Prism' a (ExprF b c)
instance AsExprF (Expr a) a (Expr a) where exprFPrism = coerced
instance AsExprF (ExprF a r) a r where exprFPrism = simple

pattern x :+: y <- (preview exprFPrism -> Just (AddF x y)) where
  x :+: y = review exprFPrism (AddF x y)
pattern x :*: y <- (preview exprFPrism -> Just (MulF x y)) where
  x :*: y = review exprFPrism (MulF x y)
pattern x :/: y <- (preview exprFPrism -> Just (DivF x y)) where
  x :/: y = review exprFPrism (DivF x y)
pattern x :%: y <- (preview exprFPrism -> Just (RemF x y)) where
  x :%: y = review exprFPrism (RemF x y)
pattern f :$: x <- (preview exprFPrism -> Just (AppF f x)) where
  f :$: x = review exprFPrism (AppF f x)
pattern x :-: y <- (preview exprFPrism -> Just (SubF x y)) where
  x :-: y = review exprFPrism (SubF x y)
pattern x :^: y <- (preview exprFPrism -> Just (PowF x y)) where
  x :^: y = review exprFPrism (PowF x y)
pattern x ://: y <- (preview exprFPrism -> Just (QutF x y)) where
  x ://: y = review exprFPrism (QutF x y)
pattern Neg x <- (preview exprFPrism -> Just (NegF x)) where
  Neg x = review exprFPrism (NegF x)
pattern Sig x <- (preview exprFPrism -> Just (SigF x)) where
  Sig x = review exprFPrism (SigF x)
pattern Abs x <- (preview exprFPrism -> Just (AbsF x)) where
  Abs x = review exprFPrism (AbsF x)
pattern Lit a <- (preview exprFPrism -> Just (LitF a)) where
  Lit a = review exprFPrism (LitF a)

class CanVar a where _Var :: Prism' a String

pattern Var n <- (preview _Var -> Just n) where
  Var = review _Var
