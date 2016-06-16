{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}

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

-- | A class for generating mathml
-- >>> renderWith True . mlRep $ (1 + 2 - 3 :: Expr Int)
-- "<apply><minus></minus><apply><plus></plus><ci>1</ci><ci>2</ci></apply><ci>3</ci></apply>"
class MathML a where mlRep :: a -> Node

cstRep :: String -> Node
cstRep = NodeElement . Element "ci" [] . pure . NodeContent . pack

instance MathML Double  where mlRep = cstRep . show
instance MathML Int     where mlRep = cstRep . show
instance MathML Integer where mlRep = cstRep . show
instance MathML Float   where mlRep = cstRep . show

instance MathML a => MathML (Expr a) where mlRep = cata mlalg
instance MathML a => MathML (VarExpr a) where
  mlRep = cata (either (cstRep.show) mlalg . getVar)

instance MathML Func where
  mlRep f = NodeElement (Element n [] []) where
    n = case f of
      Sin -> "sin"; Cos -> "cos"; Exp -> "exp"; Log -> "log"
      Tan -> "tan"; Atn -> "arctan"; Asn -> "arcsin"
      Acs -> "arccos"; Snh -> "sinh"; Csh -> "cosh"; Tnh -> "tanh"
      Ach -> "arccosh"; Ash -> "arcsinh"; Ath -> "arctanh"

mlalg :: MathML a => ExprF a 'NoVar Node -> Node
mlalg = \case
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
