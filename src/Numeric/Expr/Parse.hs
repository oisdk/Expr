{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.Expr.Parse where

import           Control.Applicative
import           GHC.Exts
import           Numeric.Expr.ExprF
import           Numeric.Expr.ExprType
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Expression
import           Text.Parser.Token
import           Text.Parser.Token.Highlight

numFuncs, signs, mult, adds :: (Monad m, TokenParsing m, ExprType e, LitType e ~ n, Num n) => [Operator m e]
numFuncs = [prefixFun "abs" Abs, prefixFun "signum" Sig, prefixFun "negate" Neg]
signs = [prefix "-" Neg, prefix "+" id]
mult = [binary "*" (:*:) AssocLeft]
adds = [binary "+" (:+:) AssocLeft, binary "-" (:-:) AssocLeft]

fracs :: (Monad m, TokenParsing m, ExprType e, LitType e ~ n, Fractional n) => [Operator m e]
fracs = [binary "/" (:/:) AssocLeft]

ints :: (Monad m, TokenParsing m, ExprType e, LitType e ~ n, Integral n) => [Operator m e]
ints = [binary "%" (:%:) AssocLeft, binary "÷" (:÷:) AssocLeft, binary "//" (:÷:) AssocLeft]

fltFuncs, exps :: (Monad m, TokenParsing m, ExprType e, LitType e ~ n, Floating n) => [Operator m e]
fltFuncs = [ prefixFun (show f) (f:$:) | f <- allFuncs]
exps = [binary "^" (:^:) AssocRight]

exprTable :: (Monad m, TokenParsing m, ExprType e, LitType e ~ n, Floating n) => [[Operator m e]]
exprTable =
  [ numFuncs ++ fltFuncs
  , signs
  , exps
  , mult ++ fracs
  , adds ]

exprParse :: (Monad m, TokenParsing m, ExprType e, LitType e ~ Double) => m e
exprParse = buildExpressionParser exprTable term <?> "expression" where
  term = parens exprParse <|> number <?> "expression"
  number = either (Lit . fromInteger) Lit <$> naturalOrDouble

intTable :: (Monad m, TokenParsing m, ExprType e, LitType e ~ n, Integral n) => [[Operator m e]]
intTable =
  [ numFuncs
  , signs
  , mult ++ ints
  , adds ]

intParse :: (Monad m, TokenParsing m, ExprType e, LitType e ~ n, Integral n) => m e
intParse = buildExpressionParser intTable term <?> "expression" where
  term = parens intParse <|> number <?> "expression"
  number = (Lit . fromInteger) <$> natural

varParse :: (Monad m, TokenParsing m, ExprType e, LitType e ~ Double, VarType e ~ 'HasVar v, IsString v, Show v) => m e
varParse = buildExpressionParser exprTable term <?> "expression" where
  term = parens varParse <|> number <?> "expression"
  number = either (Lit . fromInteger) Lit <$> naturalOrDouble <|> Var <$> ident varStyle

allFuncs :: [Func]
allFuncs = [minBound..maxBound]

binary :: (TokenParsing m, Monad m) => String -> (a -> a -> a) -> Assoc -> Operator m a
binary name fun = Infix (fun <$ reservedOp name)

prefix :: (TokenParsing m, Monad m) => String -> (a -> a) -> Operator m a
prefix name fun = Prefix (fun <$ reservedOp name)

prefixFun :: (TokenParsing m, Monad m) => String -> (a -> a) -> Operator m a
prefixFun name fun = Prefix (fun <$ reserve funcStyle name)

reservedOp :: (TokenParsing m, Monad m) => String -> m ()
reservedOp = reserve opStyle

varStyle, opStyle, funcStyle :: (Monad m, TokenParsing m) => IdentifierStyle m
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
