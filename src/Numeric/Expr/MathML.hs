{-# LANGUAGE GADTs             #-}
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
import           Text.Taggy.DOM
import           Text.Taggy.Renderer

class MathML a where mlRep :: a -> Node

cstRep :: String -> Node
cstRep = NodeElement . Element "ci" [] . pure . NodeContent . pack

instance MathML Double  where mlRep = cstRep . show
instance MathML Int     where mlRep = cstRep . show
instance MathML Integer where mlRep = cstRep . show
instance MathML Float   where mlRep = cstRep . show

instance MathML a => MathML (Expr a) where mlRep = cata (mlalg undefined)
instance MathML a => MathML (VarExpr a) where
  mlRep = cata $ \case
    VarF x -> cstRep x
    e -> mlalg undefined e

instance MathML Func where
  mlRep f = NodeElement (Element n [] []) where
    n = case f of
      Sin -> "sin"; Cos -> "cos"; Exp -> "exp"; Log -> "log"
      Tan -> "tan"; Atn -> "arctan"; Asn -> "arcsin"
      Acs -> "arccos"; Snh -> "sinh"; Csh -> "cosh"; Tnh -> "tanh"
      Ach -> "arccosh"; Ash -> "arcsinh"; Ath -> "arctanh"

mlalg :: MathML a => Node -> ExprF a v Node -> Node
mlalg d = \case
    VarF _ -> d
    LitF a -> mlRep a
    NegF x -> app [symb "minus"   , x   ]
    x :- y -> app [symb "minus"   , x, y]
    x :+ y -> app [symb "plus"    , x, y]
    x :/ y -> app [symb "divide"  , x, y]
    x :* y -> app [symb "times"   , x, y]
    x :÷ y -> app [symb "quotient", x, y]
    x :% y -> app [symb "rem"     , x, y]
    x :^ y -> app [symb "power"   , x, y]
    f :$ x -> app [mlRep f, x]
    AbsF x -> app [NodeElement (Element "abs" [] []), x]
    SigF x -> app [NodeElement (Element "signum" [] []), x]
    where
      app = NodeElement . Element "apply" []
      symb x = NodeElement (Element x [] [])

sampleML :: MathML a => Expr a -> IO ()
sampleML = putStrLn . L.unpack . renderWith True . mlRep
