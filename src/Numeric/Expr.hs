{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Numeric.Expr
  ( ExprF(..)
  , Func(..)
  , Expr
  , NumExpr(..)
  , IntExpr(..)
  , FracExpr(..)
  , pattern (:*:)
  , pattern (:+:)
  , pattern (:-:)
  , pattern (:%:)
  , pattern (:/:)
  , pattern (:$:)
  , pattern (:^:)
  , pattern (://:)
  , pattern Neg
  , pattern Abs
  , pattern Lit
  , assoc
  , simplify
  , eval
  , safeEval
  , approxEqual
  , showBracks
  ) where

import           Control.Lens hiding (para)
import           Control.Monad
import           Data.Coerce
import           Data.Function
import           Data.Functor.Foldable
import           Data.Monoid
import           Data.Ord
import           Data.Serialize
import           GHC.Generics          (Generic)
import           Test.QuickCheck

-- | An unfixed expression type, which supports most of Haskell's
-- standard numeric operations, restricting its inputs accordingly.
data ExprF a r where
  LitF :: a -> ExprF a r

  -- Num
  AddF :: Num a => r -> r -> ExprF a r
  MulF :: Num a => r -> r -> ExprF a r
  AbsF :: Num a => r -> ExprF a r
  SigF :: Num a => r -> ExprF a r
  NegF :: Num a => r -> ExprF a r

  -- Integral
  QutF :: Integral a => r -> r -> ExprF a r
  RemF :: Integral a => r -> r -> ExprF a r

  -- Fractional
  DivF :: Fractional a => r -> r -> ExprF a r

  -- Floating
  AppF :: Floating a => Func -> r -> ExprF a r

data Func =
    Sin | Cos | Exp | Log | Tan | Atn | Asn
  | Acs | Snh | Csh | Tnh | Ach | Ash | Ath
          deriving (Eq, Ord, Enum, Bounded, Generic)

instance Show Func where
  show = \case
    Exp -> "exp"
    Sin -> "sin"
    Cos -> "cos"
    Tan -> "tan"
    Log -> "log"
    Atn -> "atan"
    Snh -> "sinh"
    Csh -> "cosh"
    Tnh -> "tanh"
    Asn -> "asin"
    Acs -> "acos"
    Ach -> "acosh"
    Ash -> "asinh"
    Ath -> "atanh"

instance Arbitrary Func where arbitrary = arbitraryBoundedEnum
instance Serialize Func

-- Have to do all of these by hand, no deriving for GADTs
instance Functor (ExprF a) where
  fmap f = \case
    LitF a   -> LitF a
    AddF x y -> AddF (f x) (f y)
    MulF x y -> MulF (f x) (f y)
    AbsF x   -> AbsF (f x)
    SigF x   -> SigF (f x)
    NegF x   -> NegF (f x)
    QutF x y -> QutF (f x) (f y)
    RemF x y -> RemF (f x) (f y)
    DivF x y -> DivF (f x) (f y)
    AppF g x -> AppF g (f x)

instance Foldable (ExprF a) where
  foldr f i = \case
    LitF _   -> i
    AddF x y -> f x (f y i)
    MulF x y -> f x (f y i)
    AbsF x   -> f x i
    SigF x   -> f x i
    NegF x   -> f x i
    QutF x y -> f x (f y i)
    RemF x y -> f x (f y i)
    DivF x y -> f x (f y i)
    AppF _ x -> f x i
  foldMap f = \case
    LitF _   -> mempty
    AddF x y -> f x <> f y
    MulF x y -> f x <> f y
    AbsF x   -> f x
    SigF x   -> f x
    NegF x   -> f x
    QutF x y -> f x <> f y
    RemF x y -> f x <> f y
    DivF x y -> f x <> f y
    AppF _ x -> f x

instance Traversable (ExprF a) where
  traverse f = \case
    LitF a   -> pure (LitF a)
    AddF x y -> AddF <$> f x <*> f y
    MulF x y -> MulF <$> f x <*> f y
    AbsF x   -> AbsF <$> f x
    SigF x   -> SigF <$> f x
    NegF x   -> NegF <$> f x
    QutF x y -> QutF <$> f x <*> f y
    RemF x y -> RemF <$> f x <*> f y
    DivF x y -> DivF <$> f x <*> f y
    AppF g x -> AppF g <$> f x

instance (Eq a, Eq r) => Eq (ExprF a r) where
  (==) = zipExpr (\_ _ -> False) (==) (==) (==) (&&)

instance (Ord a, Ord r) => Ord (ExprF a r) where
  compare = zipExpr (comparing uprec) compare compare compare mappend

prec :: ExprF a r -> Int
prec = \case
  LitF _   -> 11
  AbsF _   -> 10
  SigF _   -> 10
  AppF _ _ -> 10
  QutF _ _ -> 7
  RemF _ _ -> 7
  DivF _ _ -> 7
  MulF _ _ -> 7
  AddF _ _ -> 6
  NegF _   -> 0

uprec :: ExprF a r -> Int
uprec = \case
  LitF _   -> 9
  AbsF _   -> 8
  SigF _   -> 7
  AppF _ _ -> 6
  QutF _ _ -> 5
  RemF _ _ -> 4
  DivF _ _ -> 3
  MulF _ _ -> 2
  AddF _ _ -> 1
  NegF _   -> 0

zipExpr :: (ExprF a r -> ExprF a r -> b)
        -> (a -> a -> b)
        -> (r -> r -> b)
        -> (Func -> Func -> b)
        -> (b -> b -> b)
        -> ExprF a r
        -> ExprF a r
        -> b
zipExpr d n r f c = i where
  i (LitF a  ) (LitF b  ) = n a b
  i (AddF w x) (AddF y z) = r w y `c` r x z
  i (MulF w x) (MulF y z) = r w y `c` r x z
  i (AbsF x  ) (AbsF y  ) = r x y
  i (SigF x  ) (SigF y  ) = r x y
  i (NegF x  ) (NegF y  ) = r x y
  i (QutF w x) (QutF y z) = r w y `c` r x z
  i (RemF w x) (RemF y z) = r w y `c` r x z
  i (DivF w x) (DivF y z) = r w y `c` r x z
  i (AppF w x) (AppF y z) = f w y `c` r x z
  i x y = d x y

appF :: Floating a => Func -> a -> a
appF = \case
  Exp -> exp
  Sin -> sin
  Cos -> cos
  Tan -> tan
  Log -> log
  Atn -> atan
  Snh -> sinh
  Csh -> cosh
  Tnh -> tanh
  Asn -> asin
  Acs -> acos
  Ach -> acosh
  Ash -> asinh
  Ath -> atanh

-- | A fixed expression type, which can conform to the numeric
-- typeclasses depending on what constant type it wraps.
newtype Expr a =
  Expr { _getExpr :: ExprF a (Expr a)
       } deriving (Eq, Ord)

coerceBi :: (Expr a -> Expr a -> ExprF a (Expr a))
         -> Expr a -> Expr a -> Expr a
coerceBi = coerce

coerceUn :: (Expr a -> ExprF a (Expr a)) -> Expr a -> Expr a
coerceUn = coerce

instance Num a => Num (Expr a) where
  (+) = coerceBi AddF
  (*) = coerceBi MulF
  abs = coerceUn AbsF
  signum = coerceUn SigF
  negate = coerceUn NegF
  fromInteger = Expr . LitF . fromInteger

instance Real a => Real (Expr a) where
  toRational = toRational . cata evalAlg

instance Enum a => Enum (Expr a) where
  toEnum = Expr . LitF . toEnum
  fromEnum = fromEnum . cata evalAlg

instance Integral a => Integral (Expr a) where
  toInteger = toInteger . cata evalAlg
  quotRem a b = (coerceBi QutF a b, coerceBi RemF a b)
  quot = coerceBi QutF
  rem  = coerceBi RemF

instance Fractional a => Fractional (Expr a) where
  fromRational = Expr . LitF . fromRational
  (/) = coerceBi DivF

instance Floating a => Floating (Expr a) where
  pi    = Expr . LitF $ pi
  exp   = coerceUn $ AppF Exp
  log   = coerceUn $ AppF Log
  sin   = coerceUn $ AppF Sin
  cos   = coerceUn $ AppF Cos
  asin  = coerceUn $ AppF Asn
  acos  = coerceUn $ AppF Acs
  atan  = coerceUn $ AppF Atn
  sinh  = coerceUn $ AppF Snh
  cosh  = coerceUn $ AppF Csh
  asinh = coerceUn $ AppF Ash
  acosh = coerceUn $ AppF Ach
  atanh = coerceUn $ AppF Ath

type instance Base (Expr a) = ExprF a
instance Recursive (Expr a) where project = coerce
instance Corecursive (Expr a) where embed = coerce
instance Plated (Expr a) where
  plate f = fmap embed . traverse f . project

evalAlg :: ExprF a a -> a
evalAlg = \case
  LitF a   -> a
  AddF x y -> x + y
  MulF x y -> x * y
  AbsF x   -> abs x
  SigF x   -> signum x
  NegF x   -> negate x
  QutF x y -> quot x y
  RemF x y -> rem x y
  DivF x y -> x / y
  AppF f x -> appF f x

eval :: Expr a -> a
eval = cata evalAlg

safeEvalAlg :: Eq a => ExprF a a -> Either String a
safeEvalAlg = \case
  DivF _ 0 -> Left "tried to divide by zero"
  e -> Right (evalAlg e)

-- | Evaluate an expression, catching zero-division errors.
safeEval :: Eq a => Expr a -> Either String a
safeEval = cataM safeEvalAlg

-- | A monadic catamorphism.
cataM
  :: (Recursive t, Traversable (Base t), Monad m)
  => (Base t a -> m a) -- ^ a monadic (Base t)-algebra
  -> t                 -- ^ fixed point
  -> m a               -- ^ result
cataM f = c where c = f <=< (traverse c . project)

-- | A monadic anamorphism
anaM
  :: (Corecursive t, Traversable (Base t), Monad m)
  => (a -> m (Base t a))        -- ^ a monadic (Base t)-coalgebra
  -> a                          -- ^ seed
  -> m t
anaM g = a where a = fmap embed . traverse a <=< g

litArb :: (Num a, Arbitrary a) => Gen (ExprF a r)
litArb = LitF . abs <$> arbitrary

numArb :: Num a => r -> [ExprF a r]
numArb r =
  [ AddF r r
  , MulF r r
  , AbsF r
  , SigF r
  , NegF r ]

intArb :: Integral a => r -> [ExprF a r]
intArb r =
  [ QutF r r
  , RemF r r ]

fracArb :: Fractional a => r -> [ExprF a r]
fracArb r = [ DivF r r ]

floatArb :: Floating a => r -> [Gen (ExprF a r)]
floatArb r = [ flip AppF r <$> arbitrary ]

instance (Floating a, Arbitrary a) => Arbitrary (Expr a) where
  arbitrary = sized (anaM alg) where
    alg 0 = litArb
    alg n = oneof $
      litArb :
      floatArb r ++ fmap pure (
      numArb r ++
      fracArb r
      ) where r = n `div` 2

putAlg :: Serialize a => ExprF a (PutM ()) -> PutM ()
putAlg = \case
  LitF a   -> putWord8 0 *> put a
  AbsF x   -> putWord8 1 *> x
  SigF x   -> putWord8 2 *> x
  QutF x y -> putWord8 3 *> x *> y
  RemF x y -> putWord8 4 *> x *> y
  AppF f x -> putWord8 5 *> put f *> x
  NegF x   -> putWord8 6 *> x
  DivF x y -> putWord8 7 *> x *> y
  MulF x y -> putWord8 8 *> x *> y
  AddF x y -> putWord8 9 *> x *> y

instance (Floating a, Serialize a) => Serialize (Expr a) where
  put = cata putAlg
  get = alg =<< getWord8 where
    alg = \case
      0 -> Lit   <$> get
      1 -> Abs   <$> get
      2 -> Sig   <$> get
      5 -> (:$:) <$> get <*> get
      6 -> Neg   <$> get
      7 -> (:/:) <$> get <*> get
      8 -> (:*:) <$> get <*> get
      9 -> (:+:) <$> get <*> get
      _ -> error "corrupted binary"

-- newtype wrappers for subsets of Expr , similar to QuickCheck's
-- Positive / Small, etc

-- | A subset of Expr, which only supports the operations of the Num
-- typeclass. Has different Arbitrary and Serialize instances to
-- Expr. For instance, to generate expressions with only the Num
-- typeclass operations, you might do this:
--
-- > sample $ do
-- >   NumExpr e <- arbitrary
-- >   pure e
--

newtype NumExpr a =
  NumExpr { getNumExpr :: Expr a
          } deriving (Eq, Ord, Show)

-- | A subset of Expr, which only supports the operations of the
-- Integral typeclass. Has different Arbitrary and Serialize
-- instances to Expr.
newtype IntExpr a =
  IntExpr { getIntExpr :: Expr a
          } deriving (Eq, Ord, Show)

-- | A subset of Expr, which only supports the operations of the
-- Fractional typeclass. Has different Arbitrary and Serialize
-- instances to Expr.
newtype FracExpr a =
  FracExpr { getFracExpr :: Expr a
           } deriving (Eq, Ord, Show)

instance (Num a, Arbitrary a) => Arbitrary (NumExpr a) where
  arbitrary = NumExpr <$> sized (anaM alg) where
    alg 0 = litArb
    alg n = oneof $ litArb : fmap pure (numArb r) where
      r = n `div` 2

instance (Integral a, Arbitrary a) => Arbitrary (IntExpr a) where
  arbitrary = IntExpr <$> sized (anaM alg) where
    alg 0 = litArb
    alg n = oneof $ litArb : fmap pure (numArb r ++ intArb r) where
      r = n `div` 2

instance (Fractional a, Arbitrary a) => Arbitrary (FracExpr a) where
  arbitrary = FracExpr <$> sized (anaM alg) where
    alg 0 = litArb
    alg n = oneof $ litArb : fmap pure (numArb r ++ fracArb r) where
      r = n `div` 2

instance (Num a, Serialize a) => Serialize (NumExpr a) where
  put = cata putAlg . getNumExpr
  get = getn where
    getn = coerce (alg =<< getWord8)
    gete = (coerce :: Get (NumExpr a) -> Get (Expr a)) getn
    alg = \case
      0 -> Lit   <$> get
      1 -> Abs   <$> gete
      2 -> Sig   <$> gete
      6 -> Neg   <$> gete
      8 -> (:*:) <$> gete <*> gete
      9 -> (:+:) <$> gete <*> gete
      _ -> error "corrupted binary"

instance (Integral a, Serialize a) => Serialize (IntExpr a) where
  put = cata putAlg . getIntExpr
  get = getn where
    getn = coerce (alg =<< getWord8)
    gete = (coerce :: Get (IntExpr a) -> Get (Expr a)) getn
    alg = \case
      0 -> Lit    <$> get
      1 -> Abs    <$> gete
      2 -> Sig    <$> gete
      3 -> (://:) <$> gete <*> gete
      4 -> (:%:)  <$> gete <*> gete
      6 -> Neg    <$> gete
      8 -> (:*:)  <$> gete <*> gete
      9 -> (:+:)  <$> gete <*> gete
      _ -> error "corrupted binary"

instance (Fractional a, Serialize a) => Serialize (FracExpr a) where
  put = cata putAlg . getFracExpr
  get = getn where
    getn = coerce (alg =<< getWord8)
    gete = (coerce :: Get (FracExpr a) -> Get (Expr a)) getn
    alg = \case
      0 -> Lit   <$> get
      1 -> Abs   <$> gete
      2 -> Sig   <$> gete
      6 -> Neg   <$> gete
      7 -> (:/:) <$> gete <*> gete
      8 -> (:*:) <$> gete <*> gete
      9 -> (:+:) <$> gete <*> gete
      _ -> error "corrupted binary"

pprAlg :: (Show b, Show a) => ExprF a (Expr b, ShowS) -> ShowS
pprAlg e = case e of
  LitF a   -> shows a
  NegF (c,x) -> showString "-" . showParen (11 > (prec._getExpr) c) x
  AddF x (Neg y, _) -> parL x . showString " - " . parR (y, shows y)
  AddF x y -> parL x . showString " + " . parL y
  DivF x y -> parL x . showString " / " . parR y
  MulF x y -> parL x . showString " * " . parL y
  AppF Exp ((Log :$: x) :*: y, _) ->
    parL (x, shows x) . showString " ^ " . parR (y, shows y)
  AppF f x -> shows f . showChar ' ' . parR x
  AbsF x   -> showString "abs " . parR x
  SigF x   -> showString "signum " . parR x
  QutF x y -> parL x . showString " // " . parR y
  RemF x y -> parL x . showString " % "  . parR y
  where
    parL = uncurry $ showParen . (prec e > ) . prec . _getExpr
    parR = uncurry $ showParen . (prec e >=) . prec . _getExpr

instance Show a => Show (Expr a) where showsPrec _ = para pprAlg

showBracks :: Show a => Expr a -> String
showBracks = cata $ \case
  LitF a   -> show a
  NegF x   -> '-' : p x
  AddF x y -> concat [p x, " + ", p y]
  DivF x y -> concat [p x, " / ", p y]
  MulF x y -> concat [p x, " * ", p y]
  AppF f x -> concat [show f, " ", p x]
  AbsF x   -> "abs " ++ p x
  SigF x   -> "signum " ++ p x
  QutF x y -> concat [p x, " // ", p y]
  RemF x y -> concat [p x, " % ", p y]
  where p s = concat ["(", s, ")"]

pattern x :+: y = Expr (AddF x y)
pattern x :*: y = Expr (MulF x y)
pattern x :/: y = Expr (DivF x y)
pattern x :%: y = Expr (RemF x y)
pattern x :$: y = Expr (AppF x y)
pattern x :-: y = x :+: Neg y
pattern x :^: y = Exp :$: ((Log :$: x) :*: y)
pattern x ://: y = Expr (QutF x y)
pattern Neg x = Expr (NegF x)
pattern Sig x = Expr (SigF x)
pattern Abs x = Expr (AbsF x)
pattern Lit a = Expr (LitF a)

-- | Normalizes associative operators
assoc :: Expr a -> Expr a
assoc = rewrite $ \case
    x :+: (y :+: z) -> Just $ (x :+: y) :+: z
    x :*: (y :*: z) -> Just $ (x :*: y) :*: z
    _ -> Nothing

-- | Very basic simplification
simplify :: (Num a, Eq a) => Expr a -> Expr a
simplify = rewrite $ \case
  x :+: 0 -> Just x
  0 :+: x -> Just x
  x :/: 1 -> Just x
  1 :*: x -> Just x
  x :*: 1 -> Just x
  x :^: 1 -> Just x
  1 :^: _ -> Just $ Lit 1
  _ :^: 0 -> Just $ Lit 1
  0 :*: _ -> Just $ Lit 0
  _ :*: 0 -> Just $ Lit 0
  _ :%: 1 -> Just $ Lit 0
  Neg 0   -> Just $ Lit 0
  x :-:  y | x == y -> Just $ Lit 0
  x :/:  y | x == y -> Just $ Lit 1
  x :%:  y | x == y -> Just $ Lit 0
  x ://: y | x == y -> Just $ Lit 1
  _ -> Nothing

zipo :: (Recursive f, Recursive g)
     => (Base f (g -> a) -> Base g g -> a)
     -> f -> g -> a
zipo alg = cata zalg where zalg x = alg x . project

approxEqual :: (a -> a -> Bool) -> Expr a -> Expr a -> Bool
approxEqual eq = zipo alg `on` assoc where
  alg (LitF a  ) (LitF b  ) = eq a b
  alg (AddF w x) (AddF y z) = w y && x z
  alg (MulF w x) (MulF y z) = w y && x z
  alg (AbsF x  ) (AbsF y  ) = x y
  alg (SigF x  ) (SigF y  ) = x y
  alg (NegF x  ) (NegF y  ) = x y
  alg (QutF w x) (QutF y z) = w y && x z
  alg (RemF w x) (RemF y z) = w y && x z
  alg (DivF w x) (DivF y z) = w y && x z
  alg (AppF w x) (AppF y z) = w == y && x z
  alg _ _ = False
