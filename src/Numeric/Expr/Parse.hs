{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.Expr.Parse
  ( numFuncs
  , signs
  , mult
  , adds
  , fracs
  , ints
  , fltFuncs
  , exps
  , exprParse
  , varParse
  ) where

import           Control.Applicative
import           GHC.Exts
import           Numeric.Expr.ExprF
import           Numeric.Expr.ExprType
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Expression
import           Text.Parser.Token
import           Text.Parser.Token.Highlight

-- | A class for numbers which can be parsed (not sure if this is a
-- good idea)
class ParseLit a where
  unsigned :: TokenParsing m => m a
  opstable :: (TokenParsing m, Monad m, ExprType e, LitType e ~ a)
           => [[Operator m e]]

instance ParseLit Integer where
  unsigned = natural
  opstable = intTable

instance ParseLit Int where
  unsigned = fmap fromInteger natural
  opstable = intTable

instance ParseLit Double where
  unsigned = either fromInteger id <$> naturalOrDouble
  opstable = fltTable

numFuncs, signs, mult, adds
  :: (Monad m, TokenParsing m, ExprType e, LitType e ~ n, Num n)
  => [Operator m e]
numFuncs =
  [ prefixFun "abs" Abs
  , prefixFun "signum" Sig
  , prefixFun "negate" Neg ]
signs = [prefix "-" Neg, prefix "+" id]
mult = [binary "*" (:*:) AssocLeft]
adds = [binary "+" (:+:) AssocLeft, binary "-" (:-:) AssocLeft]

fracs
  :: ( Monad m, TokenParsing m, ExprType e
     , LitType e ~ n, Fractional n)
  => [Operator m e]
fracs = [binary "/" (:/:) AssocLeft]

ints
  :: ( Monad m, TokenParsing m, ExprType e
     , LitType e ~ n, Integral n )
  => [Operator m e]
ints =
  [ binary "%" (:%:) AssocLeft
  , binary "÷" (:÷:) AssocLeft
  , binary "//" (:÷:) AssocLeft]

fltFuncs, exps
  :: (Monad m, TokenParsing m, ExprType e, LitType e ~ n, Floating n)
  => [Operator m e]
fltFuncs = [prefixFun (show f) (f:$:) | f <- allFuncs]
exps = [binary "^" (:^:) AssocRight]

fltTable
  :: (Monad m, TokenParsing m, ExprType e, LitType e ~ n, Floating n)
  => [[Operator m e]]
fltTable =
  [numFuncs ++ fltFuncs, signs, exps, mult ++ fracs, adds]

intTable
  :: (Monad m, TokenParsing m, ExprType e, LitType e ~ n, Integral n)
  => [[Operator m e]]
intTable =
  [numFuncs, signs, mult ++ ints, adds]
-- $setup
-- >>> import Text.Trifecta.Parser

-- | Parses an expression
-- >>> let doubleExpr = parseTest (exprParse :: Parser (Expr Double))
-- >>> let intExpr = parseTest (exprParse :: Parser (Expr Integer))
-- >>> doubleExpr "1"
-- 1.0
-- >>> doubleExpr "1 + 2 - 3"
-- 1.0 + 2.0 - 3.0
-- >>> intExpr "1"
-- 1
-- >>> intExpr "1 + 2 - 3"
-- 1 + 2 - 3
-- >>> intExpr "1 % 2 ÷ 3"
-- 1 % 2 ÷ 3
-- >>> intExpr "1 % (2 ÷ 3)"
-- 1 % (2 ÷ 3)
-- >>> intExpr "(1 % 2) ÷ 3"
-- 1 % 2 ÷ 3
-- >>> intExpr "1 + 2 - 3"
-- 1 + 2 - 3
-- >>> intExpr "(1 + 2) - 3"
-- 1 + 2 - 3
-- >>> intExpr "1 * (2 * 3)"
-- 1 * 2 * 3
exprParse
  :: (Monad m, TokenParsing m, ExprType e, LitType e ~ n, ParseLit n)
  => m e
exprParse =
  buildExpressionParser opstable term <?> "expression" where
    term = parens exprParse <|> number <?> "expression"
    number = fmap Lit unsigned

-- | Parses an expression which includes variables
-- >>> parseTest (varParse :: Parser (VarExpr Double)) "a + 1"
-- a + 1.0
varParse
  :: (Monad m, TokenParsing m, ExprType e
     ,LitType e ~ n, ParseLit n, VarType e ~ 'HasVar v
     ,IsString v, Show v)
  => m e
varParse = buildExpressionParser opstable term <?> "expression" where
  term = parens varParse <|> number <?> "expression"
  number = fmap Lit unsigned <|> Var <$> ident varStyle

allFuncs :: [Func]
allFuncs = [minBound..maxBound]

binary :: (TokenParsing m, Monad m)
       => String -> (a -> a -> a) -> Assoc -> Operator m a
binary name fun = Infix (fun <$ reservedOp name)

prefix :: (TokenParsing m, Monad m)
       => String -> (a -> a) -> Operator m a
prefix name fun = Prefix (fun <$ reservedOp name)

prefixFun :: (TokenParsing m, Monad m)
          => String -> (a -> a) -> Operator m a
prefixFun name fun = Prefix (fun <$ reserve funcStyle name)

reservedOp :: (TokenParsing m, Monad m) => String -> m ()
reservedOp = reserve opStyle

varStyle, opStyle, funcStyle :: (Monad m, TokenParsing m)
                             => IdentifierStyle m
varStyle =
  IdentifierStyle
    "variable"
    letter
    alphaNum
    (fromList funcNames)
    Identifier
    ReservedIdentifier

opStyle =
  IdentifierStyle
    "operator"
    ops
    ops
    (fromList ["+", "-", "^", "*", "/", "%", "÷", "//"])
    ReservedOperator
    ReservedOperator
  where ops = oneOf "+-^*/%÷"

funcStyle =
  IdentifierStyle
    "function"
    letter
    alphaNum
    (fromList funcNames)
    Identifier
    ReservedIdentifier

funcNames :: [String]
funcNames = ["abs","signum","negate"] ++ map show allFuncs
