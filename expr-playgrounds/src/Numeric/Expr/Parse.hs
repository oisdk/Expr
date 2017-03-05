{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}

module Numeric.Expr.Parse where

import           Control.Applicative
import           Data.Foldable
import           Data.Proxy
import           GHC.Exts
import           Numeric.Expr                hiding (Operator, Prefix)
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Expression
import           Text.Parser.Token
import           Text.Parser.Token.Highlight

varStyle
    :: (Monad m, TokenParsing m, KnownSymbols xs)
    => Proxy xs -> IdentifierStyle m
varStyle p =
    IdentifierStyle
        "variable"
        letter
        alphaNum
        (fromList (symbolVals p))
        Identifier
        ReservedIdentifier

symbolVals :: KnownSymbols xs => Proxy xs -> [String]
symbolVals p = map (\s -> runVarFunc s (\_ v -> v))(getSymbols p)

opStyle, funcStyle :: (Monad m, TokenParsing m) => IdentifierStyle m
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

allFuncs :: [Func]
allFuncs = [minBound..maxBound]

funcNames :: [String]
funcNames = ["abs","signum","negate"] ++ map show allFuncs

numFuncs, signs, mult, adds
  :: (Monad m, TokenParsing m, Num n)
  => [Operator m (Expr n vs)]
numFuncs =
  [ prefixFun "abs" Abs
  , prefixFun "signum" Sig
  , prefixFun "negate" Neg ]
signs = [prefix "-" Neg, prefix "+" id]
mult = [binary "*" (:*:) AssocLeft]
adds = [binary "+" (:+:) AssocLeft, binary "-" (:-:) AssocLeft]


vars :: (Monad m, TokenParsing m, KnownSymbols vs)
     => m (Expr n vs)
vars = asum (vps Proxy)
  where
    vps
        :: (TokenParsing m, Monad m, KnownSymbols vs)
        => Proxy vs -> [m (Expr n vs)]
    vps p =
        map
            (\v ->
                  runVarFunc
                      v
                      (\e s ->
                            Expr (VarF e) <$ reserve (varStyle p) s))
            (getSymbols p)

prefixFun :: (TokenParsing m, Monad m)
          => String -> (a -> a) -> Operator m a
prefixFun name fun = Prefix (fun <$ reserve funcStyle name)

prefix :: (TokenParsing m, Monad m)
       => String -> (a -> a) -> Operator m a
prefix name fun = Prefix (fun <$ reservedOp name)

reservedOp :: (TokenParsing m, Monad m) => String -> m ()
reservedOp = reserve opStyle

binary :: (TokenParsing m, Monad m)
       => String -> (a -> a -> a) -> Assoc -> Operator m a
binary name fun = Infix (fun <$ reservedOp name)

fracs
  :: ( Monad m, TokenParsing m, Fractional n)
  => [Operator m (Expr n vs)]
fracs = [binary "/" (:/:) AssocLeft]

ints
  :: ( Monad m, TokenParsing m, Integral n )
  => [Operator m (Expr n vs)]
ints =
  [ binary "%" (:%:) AssocLeft
  , binary "÷" (:÷:) AssocLeft
  , binary "//" (:÷:) AssocLeft]

fltFuncs, exps
  :: (Monad m, TokenParsing m, Floating n)
  => [Operator m (Expr n vs)]
fltFuncs = [prefixFun (show f) (f:$:) | f <- allFuncs]
exps = [binary "^" (:^:) AssocRight]

fltTable
  :: (Monad m, TokenParsing m, Floating n)
  => [[Operator m (Expr n vs)]]
fltTable =
  [numFuncs ++ fltFuncs ++ signs, exps, mult ++ fracs, adds]

intTable
  :: (Monad m, TokenParsing m, Integral n)
  => [[Operator m (Expr n vs)]]
intTable =
  [numFuncs ++ signs, mult ++ ints, adds]

expr
  :: (Monad m, TokenParsing m, Num n, KnownSymbols vs)
  => [[Operator m (Expr n vs)]] -> m (Expr n vs) -> m (Expr n vs)
expr opstable lit = go where
  go = buildExpressionParser opstable term <?> "expression" where
    term = parens go <|> number <?> "expression"
    number = lit <|> vars
