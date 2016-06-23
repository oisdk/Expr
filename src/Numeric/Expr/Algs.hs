{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Numeric.Expr.Algs where

import           Numeric.Expr.ExprF
import           Test.QuickCheck

appF :: Floating a => Func -> a -> a
appF = \case
  Exp -> exp; Sin -> sin; Cos -> cos; Tan -> tan; Log -> log
  Atn -> atan; Snh -> sinh; Csh -> cosh; Tnh -> tanh; Asn -> asin
  Acs -> acos; Ach -> acosh; Ash -> asinh; Ath -> atanh

evalAlg :: ExprF n 'NoVar n -> n
evalAlg = \case
  LitF a -> a
  x :+ y -> x + y
  x :- y -> x - y
  x :* y -> x * y
  AbsF x -> abs x
  SigF x -> signum x
  NegF x -> negate x
  x :÷ y -> quot x y
  x :% y -> rem x y
  x :/ y -> x / y
  f :$ x -> appF f x
  x :^ y -> x ** y

safeEvalAlg :: Eq n => ExprF n 'NoVar n -> Maybe n
safeEvalAlg = \case
  _ :/ 0 -> Nothing
  _ :÷ 0 -> Nothing
  _ :% 0 -> Nothing
  x -> Just $ evalAlg x

-- | An algebra for pretty-printing an expression, with
-- minimal parentheses.
pprAlg :: Show a => ExprF a v (Precedence, ShowS) -> ShowS
pprAlg e = case e of
  LitF a -> shows a
  VarF a -> shows a
  NegF x -> showChar '-' . par R x
  x :+ y -> bin " + " x y
  x :- y -> bin " - " x y
  x :/ y -> bin " / " x y
  x :* y -> bin " * " x y
  x :^ y -> bin " ^ " x y
  f :$ x -> shows f . showChar ' ' . par R x
  AbsF x -> showString "abs " . par R x
  SigF x -> showString "signum " . par R x
  x :÷ y -> bin " ÷ " x y
  x :% y -> bin " % " x y
  where
    bin s x y = par L x . showString s . par R y
    par s = uncurry $ showParen . isParens s (prec e)
    -- | Function to decide whether or not to parenthesize
    -- a given expression. Adapted from
    -- <http://www.cs.tufts.edu/%7Enr/pubs/unparse-abstract.html here>
    isParens sid (Prec oa op) (Prec ia ip) =
      ip < op || ip == op && (ia /= oa || oa /= sid)

data Precedence = Prec
  { side :: Side
  , rank :: Int }

data Side = L | R deriving Eq

-- | An algebra for pretty-printing, which conservatively
-- over-prints parentheses (for debugging)
brcAlg :: (a -> String) -> ExprF a v (Precedence, ShowS) -> ShowS
brcAlg s e = case e of
  LitF a -> showString (s a)
  VarF a -> shows a
  NegF x -> showChar '-' . par x
  x :+ y -> bin " + " x y
  x :- y -> bin " - " x y
  x :/ y -> bin " / " x y
  x :* y -> bin " * " x y
  x :^ y -> bin " ^ " x y
  f :$ x -> shows f . showChar ' ' . par x
  AbsF x -> showString "abs " . par x
  SigF x -> showString "signum " . par x
  x :÷ y -> bin " ÷ " x y
  x :% y -> bin " % " x y
  where
    bin o x y = par x . showString o . par y
    par = uncurry $ showParen . isParens (prec e)
    isParens (Prec _ op) (Prec _ ip) = ip <= op

prec :: ExprF a v r -> Precedence
prec = \case
  LitF _ -> Prec L 11
  VarF _ -> Prec L 11
  AbsF _ -> Prec L 10
  SigF _ -> Prec L 10
  NegF _ -> Prec L 10
  _ :$ _ -> Prec L 10
  _ :^ _ -> Prec R 8
  _ :÷ _ -> Prec L 7
  _ :% _ -> Prec L 7
  _ :/ _ -> Prec L 7
  _ :* _ -> Prec L 7
  _ :+ _ -> Prec L 6
  _ :- _ -> Prec L 6

litArb :: (Num a, Arbitrary a) => r -> [Gen (ExprF a v r)]
litArb = const [LitF . abs <$> arbitrary]

numArb :: Num a => r -> [Gen (ExprF a v r)]
numArb r = map pure
  [ r :+ r, r :- r, r :* r
  , AbsF r, SigF r, NegF r ]

intArb :: Integral a => r -> [Gen (ExprF a v r)]
intArb r = map pure [r :÷ r, r :% r]

fracArb :: Fractional a => r -> [Gen (ExprF a v r)]
fracArb r = [ pure (r :/ r) ]

floatArb :: Floating a => r -> [Gen (ExprF a v r)]
floatArb r = [ pure $ r :^ r, flip (:$) r <$> arbitrary ]

varArb :: (Show v, Arbitrary v) => r -> [Gen (ExprF a ('HasVar v) r)]
varArb = const [VarF <$> arbitrary]
