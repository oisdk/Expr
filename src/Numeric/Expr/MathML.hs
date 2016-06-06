{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Numeric.Expr.MathML
  ( MathML(..)
  , sampleML
  ) where

import           Data.Functor.Foldable
import           Data.Text             (pack)
import qualified Data.Text.Lazy        as L
import           Numeric.Expr.ExprF
import           Numeric.Expr.ExprType
import           Numeric.Expr.VarExpr
import           Text.Taggy.DOM
import           Text.Taggy.Renderer

class MathML a where mlRep :: a -> Node

cstRep :: String -> Node
cstRep = NodeElement . Element "ci" [] . pure . NodeContent . pack

instance MathML Double  where mlRep = cstRep . show
instance MathML Int     where mlRep = cstRep . show
instance MathML Integer where mlRep = cstRep . show
instance MathML Float   where mlRep = cstRep . show

instance MathML a => MathML (Expr a) where mlRep = cata mlalg

instance MathML Func where
  mlRep f = NodeElement (Element n [] []) where
    n = case f of
      Sin -> "sin"
      Cos -> "cos"
      Exp -> "exp"
      Log -> "log"
      Tan -> "tan"
      Atn -> "arctan"
      Asn -> "arcsin"
      Acs -> "arccos"
      Snh -> "sinh"
      Csh -> "cosh"
      Tnh -> "tanh"
      Ach -> "arccosh"
      Ash -> "arcsinh"
      Ath -> "arctanh"

instance (Num a, MathML a) => MathML (VarExpr a) where
  mlRep = cata (varExpr cstRep mlalg)

mlalg :: MathML a => ExprF a Node -> Node
mlalg = \case
    LitF a -> mlRep a
    NegF x   -> app [symb "minus"   , x   ]
    SubF x y -> app [symb "minus"   , x, y]
    AddF x y -> app [symb "plus"    , x, y]
    DivF x y -> app [symb "divide"  , x, y]
    MulF x y -> app [symb "times"   , x, y]
    QutF x y -> app [symb "quotient", x, y]
    RemF x y -> app [symb "rem"     , x, y]
    PowF x y -> app [symb "power"   , x, y]
    AppF f x -> app [mlRep f, x]
    AbsF   x -> app [NodeElement (Element "abs" [] []), x]
    SigF   x -> app [NodeElement (Element "signum" [] []), x]
    where
      app = NodeElement . Element "apply" []
      symb x = NodeElement (Element x [] [])

sampleML :: MathML a => Expr a -> IO ()
sampleML = putStrLn . L.unpack . renderWith True . mlRep
