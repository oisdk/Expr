module Numeric.Expr.Parse where

import Text.Parser.Expression
import Text.Parser.Token.Highlight
import Text.Parser.Token
import Text.Parser.Char
import Numeric.Expr.ExprType
import Text.Parser.Combinators
import Control.Applicative
import Numeric.Expr.ExprF(Func(..))
import GHC.Exts

exprTable :: (Monad m, TokenParsing m) => [[Operator m (Expr Double)]]
exprTable =
  [ [prefix "abs" Abs, prefix "signum" Sig, prefix "negate" Neg] ++ map funcP allFuncs
  , [prefix "-" Neg, prefix "+" id]
  , [binary "^" (:^:) AssocRight]
  , [binary "*" (:*:) AssocLeft, binary "/" (:/:) AssocLeft]
  , [binary "+" (:+:) AssocLeft, binary "-" (:-:) AssocLeft] ]

binary :: (TokenParsing m, Monad m) => String -> (a -> a -> a) -> Assoc -> Operator m a
binary name fun = Infix (fun <$ reservedOp name)
prefix :: (TokenParsing m, Monad m) => String -> (a -> a) -> Operator m a
prefix name fun = Prefix (fun <$ reservedOp name)

reservedOp :: (TokenParsing m, Monad m) => String -> m ()
reservedOp = reserve exprStyle

exprParse :: (Monad m, TokenParsing m) => m (Expr Double)
exprParse = buildExpressionParser exprTable term <?> "expression" where
  term = parens exprParse <|> number <?> "simple expression"
  number = either fromInteger Lit <$> naturalOrDouble

-- , binary "÷" (:÷:) AssocLeft, binary "%" (:%:) AssocLeft binary "//" (:÷:) AssocLeft
allFuncs :: [Func]
allFuncs = [minBound..maxBound]

funcP :: (Monad m, TokenParsing m) => Func -> Operator m (Expr Double)
funcP f = prefix (show f) (f:$:)

exprStyle :: (Monad m, TokenParsing m) => IdentifierStyle m
exprStyle =
  IdentifierStyle
    "expr"
    letter
    letter
    (fromList $ ["abs","signum","negate"] ++ map show allFuncs)
    Identifier
    Identifier
