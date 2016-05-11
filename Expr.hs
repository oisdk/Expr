{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Expr where

import           Control.Lens
import           Control.Monad
import           Data.Coerce
import           Data.Functor.Foldable hiding (Foldable)
import qualified Data.Functor.Foldable as Functor
import           Data.Monoid
import           Data.Ord
import           GHC.Generics          (Generic)
import Test.QuickCheck
import Data.Serialize

data ExprF a r where
  Lit :: a -> ExprF a r

  -- Num
  (:+:) :: Num a => r -> r -> ExprF a r
  (:*:) :: Num a => r -> r -> ExprF a r
  Abs   :: Num a => r -> ExprF a r
  Sig   :: Num a => r -> ExprF a r
  Neg   :: Num a => r -> ExprF a r

  -- Integral:
  Qut   :: Integral a => r -> r -> ExprF a r
  Rem   :: Integral a => r -> r -> ExprF a r

  -- Fractional:
  (:/:) :: Fractional a => r -> r -> ExprF a r

  -- Floating:
  (:$:) :: Floating a => Func -> r -> ExprF a r

data Func =
    Sin | Cos | Exp | Log | Tan | Atn | Asn
  | Acs | Snh | Csh | Tnh | Ach | Ash | Ath
          deriving (Eq, Ord, Enum, Bounded, Generic, Show)

instance Arbitrary Func where arbitrary = arbitraryBoundedEnum
instance Serialize Func

instance Functor (ExprF a) where
  fmap f = \case
    Lit a   -> Lit a
    x :+: y -> f x :+: f y
    x :*: y -> f x :*: f y
    Abs x   -> Abs (f x)
    Sig x   -> Sig (f x)
    Neg x   -> Neg (f x)
    Qut x y -> Qut (f x) (f y)
    Rem x y -> Rem (f x) (f y)
    x :/: y -> f x :/: f y
    g :$: x -> g :$: f x

instance Foldable (ExprF a) where
  foldr f i = \case
    Lit _   -> i
    x :+: y -> f x (f y i)
    x :*: y -> f x (f y i)
    Abs x   -> f x i
    Sig x   -> f x i
    Neg x   -> f x i
    Qut x y -> f x (f y i)
    Rem x y -> f x (f y i)
    x :/: y -> f x (f y i)
    _ :$: x -> f x i
  foldMap f = \case
    Lit _   -> mempty
    x :+: y -> f x <> f y
    x :*: y -> f x <> f y
    Abs x   -> f x
    Sig x   -> f x
    Neg x   -> f x
    Qut x y -> f x <> f y
    Rem x y -> f x <> f y
    x :/: y -> f x <> f y
    _ :$: x -> f x

instance Traversable (ExprF a) where
  traverse f = \case
    Lit a   -> pure (Lit a)
    x :+: y -> (:+:) <$> f x <*> f y
    x :*: y -> (:*:) <$> f x <*> f y
    Abs x   -> Abs <$> f x
    Sig x   -> Sig <$> f x
    Neg x   -> Neg <$> f x
    Qut x y -> Qut <$> f x <*> f y
    Rem x y -> Rem <$> f x <*> f y
    x :/: y -> (:/:) <$> f x <*> f y
    g :$: x -> (:$:) g <$> f x

instance (Eq a, Eq r) => Eq (ExprF a r) where
  (==) = zipExpr (\_ _ -> False) (==) (==) (==) (&&)

instance (Ord a, Ord r) => Ord (ExprF a r) where
  compare = zipExpr (comparing prec) compare compare compare mappend

prec :: ExprF a r -> Int
prec = \case
    Lit _   -> 9
    Abs _   -> 8
    Sig _   -> 7
    Qut _ _ -> 6
    Rem _ _ -> 5
    _ :$: _ -> 4
    Neg _   -> 3
    _ :/: _ -> 2
    _ :*: _ -> 1
    _ :+: _ -> 0

zipExpr :: (ExprF a r -> ExprF a r -> b)
        -> (a -> a -> b)
        -> (r -> r -> b)
        -> (Func -> Func -> b)
        -> (b -> b -> b)
        -> ExprF a r
        -> ExprF a r
        -> b
zipExpr d n r f c = i where
  i (Lit a  ) (Lit b  ) = n a b
  i (w :+: x) (y :+: z) = r w y `c` r x z
  i (w :*: x) (y :*: z) = r w y `c` r x z
  i (Abs x  ) (Abs y  ) = r x y
  i (Sig x  ) (Sig y  ) = r x y
  i (Neg x  ) (Neg y  ) = r x y
  i (Qut w x) (Qut y z) = r w y `c` r x z
  i (Rem w x) (Rem y z) = r w y `c` r x z
  i (w :/: x) (y :/: z) = r w y `c` r x z
  i (w :$: x) (y :$: z) = f w y `c` r x z
  i x y = d x y

evalAlg :: ExprF a a -> a
evalAlg = \case
    Lit a   -> a
    x :+: y -> x + y
    x :*: y -> x * y
    Abs x   -> abs x
    Sig x   -> signum x
    Neg x   -> negate x
    Qut x y -> quot x y
    Rem x y -> rem x y
    x :/: y -> x / y
    f :$: x -> appF f x

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

newtype NumExpr a =
  NumExpr { getNumExpr :: Fix (ExprF a)
          } deriving (Eq, Ord)

coerceNumBi :: (Fix (ExprF a) -> Fix (ExprF a) -> ExprF a (Fix (ExprF a))) -> NumExpr a -> NumExpr a -> NumExpr a
coerceNumBi = coerce

coerceNumUn :: (Fix (ExprF a) -> ExprF a (Fix (ExprF a))) -> NumExpr a -> NumExpr a
coerceNumUn = coerce

instance Num a => Num (NumExpr a) where
  (+) = coerceNumBi (:+:)
  (*) = coerceNumBi (:*:)
  abs = coerceNumUn Abs
  signum = coerceNumUn Sig
  negate = coerceNumUn Neg
  fromInteger = NumExpr . Fix . Lit . fromInteger

newtype IntExpr a =
  IntExpr { getIntExpr :: Fix (ExprF a)
          } deriving (Eq, Ord)

coerceIntBi :: (Fix (ExprF a) -> Fix (ExprF a) -> ExprF a (Fix (ExprF a))) -> IntExpr a -> IntExpr a -> IntExpr a
coerceIntBi = coerce

coerceIntUn :: (Fix (ExprF a) -> ExprF a (Fix (ExprF a))) -> IntExpr a -> IntExpr a
coerceIntUn = coerce

instance Num a => Num (IntExpr a) where
  (+) = coerceIntBi (:+:)
  (*) = coerceIntBi (:*:)
  abs = coerceIntUn Abs
  signum = coerceIntUn Sig
  negate = coerceIntUn Neg
  fromInteger = IntExpr . Fix . Lit . fromInteger

instance Real a => Real (IntExpr a) where
  toRational = toRational . cata evalAlg

instance Enum a => Enum (IntExpr a) where
  toEnum = IntExpr . Fix . Lit . toEnum
  fromEnum = fromEnum . cata evalAlg

instance Integral a => Integral (IntExpr a) where
  toInteger = toInteger . cata evalAlg
  quotRem a b = (coerceIntBi Qut a b, coerceIntBi Rem a b)
  quot = coerceIntBi Qut
  rem  = coerceIntBi Rem

newtype FracExpr a =
  FracExpr { getFracExpr :: Fix (ExprF a)
           } deriving (Eq, Ord)

coerceFracBi :: (Fix (ExprF a) -> Fix (ExprF a) -> ExprF a (Fix (ExprF a))) -> FracExpr a -> FracExpr a -> FracExpr a
coerceFracBi = coerce

coerceFracUn :: (Fix (ExprF a) -> ExprF a (Fix (ExprF a))) -> FracExpr a -> FracExpr a
coerceFracUn = coerce

instance Num a => Num (FracExpr a) where
  (+) = coerceFracBi (:+:)
  (*) = coerceFracBi (:*:)
  abs = coerceFracUn Abs
  signum = coerceFracUn Sig
  negate = coerceFracUn Neg
  fromInteger = FracExpr . Fix . Lit . fromInteger

instance Fractional a => Fractional (FracExpr a) where
  fromRational = FracExpr . Fix . Lit . fromRational
  (/) = coerceFracBi (:/:)

newtype FloatExpr a =
  FloatExpr { getFloatExpr :: Fix (ExprF a)
            } deriving (Eq, Ord)

coerceFloatBi :: (Fix (ExprF a) -> Fix (ExprF a) -> ExprF a (Fix (ExprF a))) -> FloatExpr a -> FloatExpr a -> FloatExpr a
coerceFloatBi = coerce

coerceFloatUn :: (Fix (ExprF a) -> ExprF a (Fix (ExprF a))) -> FloatExpr a -> FloatExpr a
coerceFloatUn = coerce

instance Num a => Num (FloatExpr a) where
  (+) = coerceFloatBi (:+:)
  (*) = coerceFloatBi (:*:)
  abs = coerceFloatUn Abs
  signum = coerceFloatUn Sig
  negate = coerceFloatUn Neg
  fromInteger = FloatExpr . Fix . Lit . fromInteger

instance Fractional a => Fractional (FloatExpr a) where
  fromRational = FloatExpr . Fix . Lit . fromRational
  (/) = coerceFloatBi (:/:)

instance Floating a => Floating (FloatExpr a) where
  pi    = (FloatExpr . Fix . Lit) pi
  exp   = coerceFloatUn $ (:$:) Exp
  log   = coerceFloatUn $ (:$:) Log
  sin   = coerceFloatUn $ (:$:) Sin
  cos   = coerceFloatUn $ (:$:) Cos
  asin  = coerceFloatUn $ (:$:) Asn
  acos  = coerceFloatUn $ (:$:) Acs
  atan  = coerceFloatUn $ (:$:) Atn
  sinh  = coerceFloatUn $ (:$:) Snh
  cosh  = coerceFloatUn $ (:$:) Csh
  asinh = coerceFloatUn $ (:$:) Ash
  acosh = coerceFloatUn $ (:$:) Ach
  atanh = coerceFloatUn $ (:$:) Ath

type instance Base (NumExpr   a) = ExprF a
type instance Base (IntExpr   a) = ExprF a
type instance Base (FracExpr  a) = ExprF a
type instance Base (FloatExpr a) = ExprF a

instance Functor.Foldable (NumExpr a) where
  project = crce project where
    crce :: (Fix (ExprF a) -> ExprF a (Fix (ExprF a))) -> NumExpr a -> ExprF a (NumExpr a)
    crce = coerce

instance Functor.Foldable (IntExpr a) where
  project = crce project where
    crce :: (Fix (ExprF a) -> ExprF a (Fix (ExprF a))) -> IntExpr a -> ExprF a (IntExpr a)
    crce = coerce

instance Functor.Foldable (FracExpr a) where
  project = crce project where
    crce :: (Fix (ExprF a) -> ExprF a (Fix (ExprF a))) -> FracExpr a -> ExprF a (FracExpr a)
    crce = coerce

instance Functor.Foldable (FloatExpr a) where
  project = crce project where
    crce :: (Fix (ExprF a) -> ExprF a (Fix (ExprF a))) -> FloatExpr a -> ExprF a (FloatExpr a)
    crce = coerce

instance Unfoldable (NumExpr a) where
  embed = crce embed where
    crce :: (ExprF a (Fix (ExprF a)) -> Fix (ExprF a)) -> ExprF a (NumExpr a) -> NumExpr a
    crce = coerce

instance Unfoldable (IntExpr a) where
  embed = crce embed where
    crce :: (ExprF a (Fix (ExprF a)) -> Fix (ExprF a)) -> ExprF a (IntExpr a) -> IntExpr a
    crce = coerce

instance Unfoldable (FracExpr a) where
  embed = crce embed where
    crce :: (ExprF a (Fix (ExprF a)) -> Fix (ExprF a)) -> ExprF a (FracExpr a) -> FracExpr a
    crce = coerce

instance Unfoldable (FloatExpr a) where
  embed = crce embed where
    crce :: (ExprF a (Fix (ExprF a)) -> Fix (ExprF a)) -> ExprF a (FloatExpr a) -> FloatExpr a
    crce = coerce

instance Plated (NumExpr   a) where
  plate f = fmap embed . traverse f . project

instance Plated (IntExpr   a) where
  plate f = fmap embed . traverse f . project

instance Plated (FracExpr  a) where
  plate f = fmap embed . traverse f . project

instance Plated (FloatExpr a) where
  plate f = fmap embed . traverse f . project

-- | A monadic catamorphism.
cataM
  :: (Functor.Foldable t, Traversable (Base t), Monad m)
  => (Base t a -> m a) -- ^ a monadic (Base t)-algebra
  -> t                 -- ^ fixed point
  -> m a               -- ^ result
cataM f = c where c = f <=< (traverse c . project)

-- | A monadic anamorphism
anaM
  :: (Unfoldable t, Traversable (Base t), Monad m)
  => (a -> m (Base t a))        -- ^ a monadic (Base t)-coalgebra
  -> a                          -- ^ seed
  -> m t
anaM g = a where a = fmap embed . traverse a <=< g

zipo :: (Functor.Foldable g, Functor.Foldable h)
     => (Base g (h -> c) -> Base h h -> c) -- ^ An algebra for two Foldables
     -> g                                  -- ^ first fixed point
     -> h                                  -- ^ second
     -> c                                  -- ^ result
zipo alg = cata zalg where zalg x = alg x . project

litArb :: Arbitrary a => Gen (ExprF a r)
litArb = fmap Lit arbitrary

numArb :: Num a => r -> [ExprF a r]
numArb r =
  [ r :+: r
  , r :*: r
  , Abs r
  , Sig r
  , Neg r ]

intArb :: Integral a => r -> [ExprF a r]
intArb r =
  [ Qut r r
  , Rem r r ]

fracArb :: Fractional a => r -> [ExprF a r]
fracArb r = [ r :/: r ]

floatArb :: Floating a => r -> [Gen (ExprF a r)]
floatArb r = [flip (:$:) r <$> arbitrary]

instance (Arbitrary a, Num a) => Arbitrary (NumExpr a) where
  arbitrary = sized (anaM alg) where
    alg 0 = litArb
    alg n = oneof $ litArb : fmap pure (numArb r) where
      r = n `div` 2

instance (Arbitrary a, Integral a) => Arbitrary (IntExpr a) where
  arbitrary = sized (anaM alg) where
    alg 0 = litArb
    alg n = oneof $ litArb : fmap pure (numArb r ++ intArb r) where
      r = n `div` 2

instance (Arbitrary a, Fractional a) => Arbitrary (FracExpr a) where
  arbitrary = sized (anaM alg) where
    alg 0 = litArb
    alg n = oneof $ litArb : fmap pure (numArb r ++ fracArb r) where
      r = n `div` 2

instance (Arbitrary a, Floating a) => Arbitrary (FloatExpr a) where
  arbitrary = sized (anaM alg) where
    alg 0 = litArb
    alg n = oneof $ litArb : floatArb r ++ fmap pure (numArb r ++ fracArb r) where
      r = n `div` 2

putAlg :: Serialize a => ExprF a (PutM ()) -> PutM ()
putAlg = \case
  Lit a   -> putWord8 0 *> put a
  Abs x   -> putWord8 1 *> x
  Sig x   -> putWord8 2 *> x
  Qut x y -> putWord8 3 *> x *> y
  Rem x y -> putWord8 4 *> x *> y
  f :$: x -> putWord8 5 *> put f *> x
  Neg x   -> putWord8 6 *> x
  x :/: y -> putWord8 7 *> x *> y
  x :*: y -> putWord8 8 *> x *> y
  x :+: y -> putWord8 9 *> x *> y

instance (Num a, Serialize a) => Serialize (NumExpr a) where
  put = cata putAlg
  get = anaM (const $ alg =<< getWord8) () where
    alg = \case
      0 -> Lit <$> get
      1 -> pure $ Abs ()
      2 -> pure $ Sig ()
      6 -> pure $ Neg ()
      8 -> pure $ () :*: ()
      9 -> pure $ () :+: ()
      _ -> error "corrupted binary"

instance (Integral a, Serialize a) => Serialize (IntExpr a) where
  put = cata putAlg
  get = anaM (const $ alg =<< getWord8) () where
    alg = \case
      0 -> Lit <$> get
      1 -> pure $ Abs ()
      2 -> pure $ Sig ()
      3 -> pure $ Qut () ()
      4 -> pure $ Rem () ()
      6 -> pure $ Neg ()
      8 -> pure $ () :*: ()
      9 -> pure $ () :+: ()
      _ -> error "corrupted binary"

instance (Fractional a, Serialize a) => Serialize (FracExpr a) where
  put = cata putAlg
  get = anaM (const $ alg =<< getWord8) () where
    alg = \case
      0 -> Lit <$> get
      1 -> pure $ Abs ()
      2 -> pure $ Sig ()
      6 -> pure $ Neg ()
      7 -> pure $ () :/: ()
      8 -> pure $ () :*: ()
      9 -> pure $ () :+: ()
      _ -> error "corrupted binary"

instance (Floating a, Serialize a) => Serialize (FloatExpr a) where
  put = cata putAlg
  get = anaM (const $ alg =<< getWord8) () where
    alg = \case
      0 -> Lit <$> get
      1 -> pure $ Abs ()
      2 -> pure $ Sig ()
      5 -> (:$:) <$> get ?? ()
      6 -> pure $ Neg ()
      7 -> pure $ () :/: ()
      8 -> pure $ () :*: ()
      9 -> pure $ () :+: ()
      _ -> error "corrupted binary"

pprAlg :: Show a => ExprF a (Int, ShowS) -> ShowS
pprAlg e = case e of
  Lit a   -> shows a
  Neg x   -> showString "-" . parR x
  x :+: y -> parL x . showString " + " . parL y
  x :/: y -> parL x . showString " / " . parR y
  x :*: y -> parL x . showString " * " . parL y
  f :$: (_,x) -> shows f . showChar '(' . x . showChar ')'
  Abs (_,x) -> showString "Abs(" . x . showChar ')'
  Sig (_,x) -> showString "Sig(" . x . showChar ')'
  Qut (_,x) (_,y) -> showString "Quot(" . x . showString ", " . y . showChar ')'
  Rem (_,x) (_,y) -> showString "Rem(" . x . showString ", " . y . showChar ')'
  where
    parL (c,p) = showParen (prec e >  c) p
    parR (c,p) = showParen (prec e >= c) p

instance Show a => Show (NumExpr a) where
  showsPrec _ = zygo prec pprAlg

instance Show a => Show (IntExpr a) where
  showsPrec _ = zygo prec pprAlg

instance Show a => Show (FracExpr a) where
  showsPrec _ = zygo prec pprAlg

instance Show a => Show (FloatExpr a) where
  showsPrec _ = zygo prec pprAlg
