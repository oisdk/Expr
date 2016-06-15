{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.Expr.Parse where

import Text.Parser.Expression
import Text.Parser.Token.Highlight
import Text.Parser.Token
import Text.Parser.Char
import Numeric.Expr.ExprType
import Numeric.Expr.ExprF
import Text.Parser.Combinators
import Control.Applicative
import GHC.Exts

numFuncs, signs, adds, mult :: (Monad m, TokenParsing m, ExprType e, LitType e ~ n, Num n) => [Operator m e]
numFuncs = [prefix "abs" Abs, prefix "signum" Sig, prefix "negate" Neg]
signs = [prefix "-" Neg, prefix "+" id]
adds = [binary "+" (:+:) AssocLeft, binary "-" (:-:) AssocLeft]
mult = [binary "*" (:*:) AssocLeft]

fltFuncs, exps :: (Monad m, TokenParsing m, ExprType e, LitType e ~ n, Floating n) => [Operator m e]
fltFuncs = [prefix (show f) (f:$:) | f <- [minBound..maxBound]]
exps = [binary "^" (:^:) AssocRight]

fracs :: (Monad m, TokenParsing m, ExprType e, LitType e ~ n, Fractional n) => [Operator m e]
fracs = [binary "/" (:/:) AssocLeft]

ints :: (Monad m, TokenParsing m, ExprType e, LitType e ~ n, Integral n) => [Operator m e]
ints = [binary "%" (:%:) AssocLeft, binary "÷" (:÷:) AssocLeft, binary "//" (:÷:) AssocLeft]

binary :: (TokenParsing m, Monad m) => String -> (a -> a -> a) -> Assoc -> Operator m a
binary name fun = Infix (fun <$ reservedOp name)

prefix :: (TokenParsing m, Monad m) => String -> (a -> a) -> Operator m a
prefix name fun = Prefix (fun <$ reservedOp name)

reservedOp :: (TokenParsing m, Monad m) => String -> m ()
reservedOp = reserve exprStyle

exprStyle :: (Monad m, TokenParsing m) => IdentifierStyle m
exprStyle =
  IdentifierStyle
    "expr"
    letter
    alphaNum
    (fromList $ ["abs","signum","negate"] ++ map show allFuncs)
    Identifier
    ReservedIdentifier
  where
    allFuncs :: [Func]
    allFuncs = [minBound..maxBound]

exprTable :: (Monad m, TokenParsing m) => [[Operator m (Expr Double)]]
exprTable =
  [ [prefix "abs" Abs, prefix "signum" Sig, prefix "negate" Neg] ++ [prefix (show f) (f:$:) | f <- allFuncs]
  , [prefix "-" Neg, prefix "+" id]
  , [binary "^" (:^:) AssocRight]
  , [binary "*" (:*:) AssocLeft, binary "/" (:/:) AssocLeft]
  , [binary "+" (:+:) AssocLeft, binary "-" (:-:) AssocLeft] ] where
  allFuncs :: [Func]
  allFuncs = [minBound..maxBound]
  -- funcP f = prefix (show f) (f:$:)
-- exprTable =
--   [ numFuncs ++ fltFuncs
--   , [prefix "-" Neg, prefix "+" id]
--   , exps
--   , mult ++ fracs
--   , adds ]

-- exprParse :: (Monad m, TokenParsing m) => m (Expr Double)
-- exprParse = buildExpressionParser exprTable term <?> "expression" where
--   term = parens exprParse <|> number <?> "simple expression"
--   number = either (Lit . fromInteger) Lit <$> naturalOrDouble
exprParse :: (Monad m, TokenParsing m) => m (Expr Double)
exprParse = buildExpressionParser exprTable term <?> "expression" where
  term = parens exprParse <|> number <?> "simple expression"
  number = either fromInteger Lit <$> naturalOrDouble

intTable :: (Monad m, TokenParsing m, ExprType e, LitType e ~ n, Integral n) => [[Operator m e]]
intTable =
  [ numFuncs
  , signs
  , mult ++ ints
  , adds ]

intParse :: (Monad m, TokenParsing m, ExprType e, LitType e ~ n, Integral n) => m e
intParse = buildExpressionParser intTable term <?> "expression" where
  term = parens intParse <|> number <?> "simple expression"
  number = (Lit . fromInteger) <$> natural

-- varParse :: (Monad m, TokenParsing m, ExprType e, LitType e ~ Double, VarType e ~ ('HasVar String)) => m e
-- varParse = buildExpressionParser exprTable term <?> "expression" where
--   term = parens varParse <|> number <?> "simple expression"
--   number = either (Lit . fromInteger) Lit <$> naturalOrDouble <|> Var <$> ident exprStyle
