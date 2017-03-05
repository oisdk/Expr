{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

module Numeric.Expr where


import           Data.Functor.Foldable
import           Data.Proxy
import           GHC.TypeLits
import Data.Functor.Classes
import Control.Lens hiding (Contains(..))
import Data.Monoid (Endo(..), (<>))
import Control.Arrow

data Elem (x :: k) (xs :: [k]) where
  Here :: Elem x (x : ys)
  There :: Elem x ys -> Elem x (y : ys)

type family EqHead (x :: k) (xs :: [k]) where
  EqHead x (x : xs) = 'True
  EqHead x (y : xs) = 'False

class ContainsD (b :: Bool) (x :: k) (xs :: [k]) | xs b -> x where
  contains' :: Proxy b -> Elem x xs

instance (EqHead x (y : xs) ~ True, x ~ y) => ContainsD True x (y : xs) where
  contains' _ = Here

instance (EqHead x (y : xs) ~ False, Contains x xs) => ContainsD False x (y : xs) where
  contains' _ = There contains

class Contains (x :: k) (xs :: [k]) where
  contains :: Elem x xs

instance (b ~ EqHead x (y : xs), ContainsD b x (y : xs)) => Contains x (y : xs) where
  contains = contains' (Proxy :: Proxy (EqHead x (y : xs)))

class Remove x xs ys | x xs -> ys where
  replace :: Proxy x -> Elem y xs -> Maybe (Elem y ys)

instance Remove x (x : xs) xs where
  replace _ Here       = Nothing
  replace _ (There ys) = Just ys

instance Remove x xs ys => Remove x (y : xs) (y : ys) where
  replace _ Here       = Just Here
  replace p (There xs) = fmap There (replace p xs)

-- | Unfixed expression type
data ExprF n vs r where
  VarF :: (KnownSymbol var) => Elem var vs -> ExprF n vs r

  -- Num
  (:+) :: Num n => r -> r -> ExprF n vs r
  (:-) :: Num n => r -> r -> ExprF n vs r
  (:*) :: Num n => r -> r -> ExprF n vs r
  AbsF  :: Num n => r -> ExprF n vs r
  SigF  :: Num n => r -> ExprF n vs r
  NegF  :: Num n => r -> ExprF n vs r
  LitF  :: Num n => Integer -> ExprF n vs r

  -- Integral
  (:÷) :: Integral n => r -> r -> ExprF n vs r
  (:%) :: Integral n => r -> r -> ExprF n vs r

    -- Fractional
  (:/) :: Fractional n => r -> r -> ExprF n v r
  RatF :: Fractional n => Rational -> ExprF n v r

  -- Floating
  (:$) :: Floating n => Func -> r -> ExprF n v r
  (:^) :: Floating n => r -> r -> ExprF n v r
  PiF :: Floating n => ExprF n v r

deriving instance Functor (ExprF n vs)
deriving instance Foldable (ExprF n vs)
deriving instance Traversable (ExprF n vs)

zipExpr :: (Integer -> Integer -> res)
        -> (Rational -> Rational -> res)
        -> (Func -> Func -> res)
        -> (forall x y. (KnownSymbol x, KnownSymbol y) => Elem x v -> Elem y v -> res)
        -> (ra -> rb -> res)
        -> (res -> res -> res)
        -> res
        -> res
        -> ExprF lit v ra
        -> ExprF lit v rb
        -> res
zipExpr l i f v r c t d = (~=) where
  PiF ~= PiF = t
  RatF a ~= RatF b = i a b
  LitF a ~= LitF b = l a b
  (w :+ x) ~= (y :+ z) = r w y `c` r x z
  (w :- x) ~= (y :- z) = r w y `c` r x z
  (w :^ x) ~= (y :^ z) = r w y `c` r x z
  (w :* x) ~= (y :* z) = r w y `c` r x z
  AbsF x   ~= AbsF y   = r x y
  SigF x   ~= SigF y   = r x y
  NegF x   ~= NegF y   = r x y
  (w :÷ x) ~= (y :÷ z) = r w y `c` r x z
  (w :% x) ~= (y :% z) = r w y `c` r x z
  (w :/ x) ~= (y :/ z) = r w y `c` r x z
  (w :$ x) ~= (y :$ z) = f w y `c` r x z
  VarF x ~= VarF y = v x y
  _ ~= _ = d

prec :: ExprF l v r -> Int
prec = \case
  PiF -> 14; RatF _ -> 13
  LitF _ -> 12; VarF _ -> 11; _ :+ _ -> 10; _ :- _ -> 9; _ :* _ -> 8
  AbsF _ -> 7; SigF _ -> 6; NegF _ -> 5; _ :÷ _ -> 4; _ :% _ -> 3
  _ :/ _ -> 2; _ :$ _ -> 1; _ :^ _ -> 0

instance Eq1 (ExprF n vs) where
  liftEq eq = zipExpr (==) (==) (==) cmp eq (&&) True False where
    cmp :: forall x y v. Elem x v -> Elem y v -> Bool
    cmp Here Here = True
    cmp (There _) Here = False
    cmp Here (There _) = False
    cmp (There x) (There y) = cmp x y

instance Ord1 (ExprF n vs) where
  liftCompare cmp xs ys = zipExpr compare compare compare vs cmp mappend EQ (compare (prec xs) (prec ys)) xs ys where
    vs :: forall x y v. Elem x v -> Elem y v -> Ordering
    vs Here Here = EQ
    vs (There _) Here = LT
    vs Here (There _) = GT
    vs (There x) (There y) = vs x y

type instance Base (Expr n vs) = ExprF n vs
newtype Expr n vs = Expr { getExpr :: ExprF n vs (Expr n vs) }
instance Recursive (Expr n vs) where
  project = getExpr
instance Corecursive (Expr n vs) where
  embed = Expr
instance Eq (Expr n vs) where
  Expr xs == Expr ys = eq1 xs ys
instance Ord (Expr n vs) where
  compare (Expr xs) (Expr ys) = compare1 xs ys

evalAlg :: ExprF n '[] n -> n
evalAlg = \case
  PiF -> pi
  RatF x -> fromRational x
  LitF a -> fromInteger a
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
  VarF e -> case e of {}

appF :: Floating a => Func -> a -> a
appF = \case
  Exp -> exp; Sin -> sin; Cos -> cos; Tan -> tan; Log -> log
  Atn -> atan; Snh -> sinh; Csh -> cosh; Tnh -> tanh; Asn -> asin
  Acs -> acos; Ach -> acosh; Ash -> asinh; Ath -> atanh


data Func =
    Sin | Cos | Exp | Log | Tan | Atn | Asn
  | Acs | Snh | Csh | Tnh | Ach | Ash | Ath
          deriving (Eq, Ord, Enum, Bounded)

instance Show Func where
  show = \case
    Exp -> "exp"; Sin -> "sin"; Cos -> "cos"; Tan -> "tan";
    Log -> "log"; Atn -> "atan"; Snh -> "sinh"; Csh -> "cosh"
    Tnh -> "tanh"; Asn -> "asin"; Acs -> "acos"; Ach -> "acosh"
    Ash -> "asinh"; Ath -> "atanh"

instance Num n => Num (Expr n vs) where
  (+) = (:+:)
  (*) = (:*:)
  abs = Abs
  signum = Sig
  negate = Neg
  fromInteger = Lit
  (-) = (:-:)

instance Real a => Real (Expr a '[]) where
  toRational = toRational . cata evalAlg

instance (Num a, Enum a) => Enum (Expr a '[]) where
  toEnum = Lit . toEnum
  fromEnum = fromEnum . cata evalAlg

instance Integral a => Integral (Expr a '[]) where
  toInteger = toInteger . cata evalAlg
  quotRem x y = (x :÷: y, x :%: y)
  quot = (:÷:)
  rem = (:%:)

instance Fractional a => Fractional (Expr a v) where
  (/) = (:/:)
  fromRational = Rat

instance Floating a => Floating (Expr a v) where
  pi = Pi
  exp   = (:$:) Exp
  log   = (:$:) Log
  sin   = (:$:) Sin
  cos   = (:$:) Cos
  asin  = (:$:) Asn
  acos  = (:$:) Acs
  atan  = (:$:) Atn
  sinh  = (:$:) Snh
  cosh  = (:$:) Csh
  asinh = (:$:) Ash
  acosh = (:$:) Ach
  atanh = (:$:) Ath
  (**)  = (:^:)

var :: (Contains var vs, KnownSymbol var) => Proxy var -> Expr n vs
var (_ :: Proxy var) = Expr (VarF (contains :: Contains var vs => Elem var vs)) :: Contains var vs => Expr n vs

matchVar :: (Contains var vs, KnownSymbol var) => Expr n vs -> Maybe (Proxy var)
matchVar (Expr (VarF (vs :: Elem v vs))) = run vs (contains :: Contains var vs => Elem var vs) where
  run :: forall x y vars. Elem x vars -> Elem y vars -> Maybe (Proxy y)
  run Here Here = Just Proxy
  run Here (There _) = Nothing
  run (There _) Here = Nothing
  run (There x) (There y) = run x y
matchVar _ = Nothing

pattern x :*: y = Expr (x :* y)
pattern x :+: y = Expr (x :+ y)
pattern x :-: y = Expr (x :- y)
pattern x :/: y = Expr (x :/ y)
pattern x :%: y = Expr (x :% y)
pattern x :÷: y = Expr (x :÷ y)
pattern x :^: y = Expr (x :^ y)
pattern x :$: y = Expr (x :$ y)
pattern Pi = Expr PiF
pattern Abs x = Expr (AbsF x)
pattern Sig x = Expr (SigF x)
pattern Lit x = Expr (LitF x)
pattern Rat x = Expr (RatF x)
pattern Neg x = Expr (NegF x)

pattern Var :: (Contains var vs, KnownSymbol var) => Proxy var -> Expr n vs
pattern Var x <- (matchVar -> Just x) where
  Var x = var x

_RemVar
    :: (Remove x xs ys)
    => Proxy x -> Setter (Expr n xs) (Expr n ys) (Proxy x) (Expr n ys)
_RemVar _ (f :: Proxy x -> f (Expr n ys)) =
    let v = f Proxy
    in cata $
       \case
           PiF -> pure Pi
           RatF a -> pure (Rat a)
           x :+ y -> (:+:) <$> x <*> y
           x :* y -> (:*:) <$> x <*> y
           x :- y -> (:-:) <$> x <*> y
           AbsF x -> fmap Abs x
           SigF x -> fmap Sig x
           NegF x -> fmap Neg x
           LitF x -> pure (Lit x)
           x :÷ y -> (:÷:) <$> x <*> y
           x :% y -> (:%:) <$> x <*> y
           x :/ y -> (:/:) <$> x <*> y
           fn :$ x -> fmap (fn :$:) x
           x :^ y -> (:^:) <$> x <*> y
           VarF (q :: Elem y xs) ->
               case (replace (Proxy :: Proxy x) q :: Maybe (Elem y ys)) of
                   Nothing -> v
                   Just vs -> pure (Expr (VarF vs))

_Var
    :: (Contains x xs, Contains y xs, KnownSymbol y)
    => Proxy x -> Traversal (Expr n xs) (Expr n xs) (Proxy x) (Proxy y)
_Var p (f :: Proxy x -> f (Proxy y)) =
    let v = fmap (Expr . VarF . toElem) (f Proxy)
    in cata $
       \case
           PiF -> pure Pi
           RatF a -> pure (Rat a)
           x :+ y -> (:+:) <$> x <*> y
           x :* y -> (:*:) <$> x <*> y
           x :- y -> (:-:) <$> x <*> y
           AbsF x -> fmap Abs x
           SigF x -> fmap Sig x
           NegF x -> fmap Neg x
           LitF x -> pure (Lit x)
           x :÷ y -> (:÷:) <$> x <*> y
           x :% y -> (:%:) <$> x <*> y
           x :/ y -> (:/:) <$> x <*> y
           fn :$ x -> fmap (fn :$:) x
           x :^ y -> (:^:) <$> x <*> y
           VarF q -> rep q (toElem p) v (pure (Expr (VarF q)))
               where rep
                         :: forall xv yv xs a.
                            Elem xv xs -> Elem yv xs -> a -> a -> a
                     rep Here Here t _ = t
                     rep (There x) (There y) t n = rep x y t n
                     rep _ _ _ n = n

toElem :: Contains x xs => Proxy x -> Elem x xs
toElem _ = contains

example :: Expr Integer '["a","b"]
example = 1 + 2 * Var (Proxy @ "a")

instance Show (Expr n vs) where
    showsPrec n =
        showParen (n >= 1) .
        appEndo . showExpr (Endo . showParen True . appEndo) (proj . getExpr)
      where
        proj =
            \case
                RatF x -> PrintLit (ff x)
                PiF -> PrintLit (Endo (showChar 'ᴨ'))
                LitF x -> PrintLit (ff x)
                VarF (_ :: Elem var vs) ->
                    PrintLit (fs (symbolVal (Proxy :: Proxy var)))
                AbsF x -> Prefix (Operator L 10 (fs "abs ")) x
                SigF x -> Prefix (Operator L 10 (fs "signum ")) x
                NegF x -> Prefix (Operator L 10 (fs "-")) x
                f :$ x -> Prefix (Operator L 10 (ff f <> fs " ")) x
                x :^ y -> Binary (Operator R 8 (fs " ^ ")) x y
                x :÷ y -> Binary (Operator L 7 (fs " ÷ ")) x y
                x :% y -> Binary (Operator L 7 (fs " % ")) x y
                x :/ y -> Binary (Operator L 7 (fs " / ")) x y
                x :* y -> Binary (Operator L 7 (fs " * ")) x y
                x :+ y -> Binary (Operator L 6 (fs " + ")) x y
                x :- y -> Binary (Operator L 6 (fs " - ")) x y
        fs = Endo . showString
        ff
            :: forall a.
               Show a
            => a -> Endo String
        ff = Endo . shows

data Side = L | R deriving Eq

-- | This datatype represents a level of an expression tree, and it
-- contains all the information needed to properly print that given
-- expression.
data ShowExpr t e
  -- | An expression with no children. The argument is the expression's textual representation.
  = PrintLit t
  -- | A prefix expression with one child.
  | Prefix  (Operator t) e
  -- | A postfix expression with one child.
  | Postfix (Operator t) e
  -- | An expression with two children.
  | Binary (Operator t) e e
  deriving  Functor

-- | This datatype represents the necessary information for pretty-printing an operator
data Operator t = Operator
  { -- | The associativity of an operator. Most are left-associative. Exponentiation is one of the exceptions.
    _associativity  :: Side
    -- | Precedence is assumed to be unique.
  , _precedence     :: Int
    -- | The textual representation of the operator.
  , _representation :: t }

showExpr :: Monoid t
         => (t -> t) -- ^ This argument should be a function which parenthesizes its input.
         -> (e -> ShowExpr t e)
         -> e -> t
showExpr prns projc = rec . projc where
  rec = showAlg . fmap ((prec' &&& rec) . projc)
  showAlg = \case
    PrintLit t                           ->                     t
    Prefix  (Operator s r t)       (q,y) ->                     t <> ifPrns R s r q y
    Postfix (Operator s r t) (p,x)       -> ifPrns L s r p x <> t
    Binary  (Operator s r t) (p,x) (q,y) -> ifPrns L s r p x <> t <> ifPrns R s r q y
  ifPrns sid oa op' (Just (ia,ip))
    | ip < op' || ip == op' && (ia /= oa || oa /= sid) = prns
  ifPrns _ _ _ _ = id
  prec' = \case
    PrintLit _                   -> Nothing
    Prefix  (Operator s r _) _   -> Just (s,r)
    Postfix (Operator s r _) _   -> Just (s,r)
    Binary  (Operator s r _) _ _ -> Just (s,r)
{-# INLINABLE showExpr #-}

instance Plated (Expr n vs) where
  plate f (Expr xs) = fmap Expr (traverse f xs)

assoc :: Expr n vs -> Expr n vs
assoc = rewrite $ \case
  x :*: (y :*: z) -> Just $ (x :*: y) :*: z
  x :*: (y :/: z) -> Just $ (x :*: y) :/: z
  x :+: (y :-: z) -> Just $ (x :+: y) :-: z
  x :+: (y :+: z) -> Just $ (x :+: y) :+: z
  _               -> Nothing

simplify :: Expr n vs -> Expr n vs
simplify = rewrite $ \case
  x :+: 0 -> Just x
  0 :+: x -> Just x
  x :/: 1 -> Just x
  1 :*: x -> Just x
  x :*: 1 -> Just x
  x :^: 1 -> Just x
  1 :^: _ -> Just 1
  _ :^: 0 -> Just 1
  0 :*: _ -> Just 0
  _ :*: 0 -> Just 0
  _ :%: 1 -> Just 0
  Neg 0   -> Just 0
  x :-: y | x == y -> Just 0
  x :/: y | x == y -> Just 1
  x :%: y | x == y -> Just 0
  x :÷: y | x == y -> Just 1
  _ -> Nothing

safeEvalAlg :: Eq n => ExprF n '[] n -> Maybe n
safeEvalAlg = \case
  _ :/ 0 -> Nothing
  _ :÷ 0 -> Nothing
  _ :% 0 -> Nothing
  x -> Just $ evalAlg x

class KnownSymbols (xs :: [Symbol]) where
  getSymbols :: Proxy xs -> [VarFunc xs]

instance KnownSymbols '[] where
  getSymbols _ = []

instance (KnownSymbol x, KnownSymbols xs) =>
         KnownSymbols (x : xs) where
    getSymbols (Proxy :: Proxy (x : xs)) =
        fromSymbol (Proxy :: Proxy x) : map mapVarFunc (getSymbols (Proxy :: Proxy xs))

newtype VarFunc vs = VarFunc
  { runVarFunc :: forall b. (forall v. KnownSymbol v => Elem v vs -> String -> b) -> b }

mapVarFunc :: VarFunc vs -> VarFunc (v : vs)
mapVarFunc (VarFunc f) = VarFunc (\c -> f (\y -> c (There y)))

fromSymbol :: KnownSymbol v => Proxy v -> VarFunc (v : vs)
fromSymbol (p :: Proxy v) = VarFunc (\c -> c (Here :: Elem v (v : vs)) (symbolVal p))
