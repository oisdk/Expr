{-# LANGUAGE PatternSynonyms #-}

module Numeric.Expr.Synonyms where

import Numeric.Expr.ExprType
import Numeric.Expr.ExprF

pattern x :+: y = Expr (AddF x y)
pattern x :*: y = Expr (MulF x y)
pattern x :/: y = Expr (DivF x y)
pattern x :%: y = Expr (RemF x y)
pattern x :$: y = Expr (AppF x y)
pattern x :-: y = Expr (SubF x y)
pattern x :^: y = Expr (PowF x y)
pattern x ://: y = Expr (QutF x y)
pattern Neg x = Expr (NegF x)
pattern Sig x = Expr (SigF x)
pattern Abs x = Expr (AbsF x)
pattern Lit a = Expr (LitF a)
