{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE PatternSynonyms  #-}

module Expr
  ( ExprF(..)
  , NumExpr
  , IntExpr
  , FracExpr
  , FloatExpr
  , Func(..)
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
  ) where

import           Control.Lens
import           Control.Monad
import           Data.Coerce
import           Data.Functor.Foldable hiding (Foldable)
import qualified Data.Functor.Foldable as Functor
import           Data.Monoid
import           Data.Ord
import           Data.Serialize
import           GHC.Generics          (Generic)
import           Test.QuickCheck

data ExprF a r where
  LitF :: a -> ExprF a r

  -- Num
  AddF :: Num a => r -> r -> ExprF a r
  MulF :: Num a => r -> r -> ExprF a r
  AbsF :: Num a => r -> ExprF a r
  SigF :: Num a => r -> ExprF a r
  NegF :: Num a => r -> ExprF a r

  -- Integral:
  QutF :: Integral a => r -> r -> ExprF a r
  RemF :: Integral a => r -> r -> ExprF a r

  -- Fractional:
  DivF :: Fractional a => r -> r -> ExprF a r

  -- Floating:
  AppF :: Floating a => Func -> r -> ExprF a r

type Expr a = Fix (ExprF a)

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
  compare = zipExpr (comparing prec) compare compare compare mappend

prec :: ExprF a r -> Int
prec = \case
    LitF _    -> 9
    AbsF _   -> 8
    SigF _   -> 7
    AppF _ _ -> 6
    NegF _   -> 5
    QutF _ _ -> 4
    RemF _ _ -> 3
    DivF _ _ -> 2
    MulF _ _ -> 1
    AddF _ _ -> 0

zipExpr :: (ExprF a r -> ExprF a r -> b)
        -> (a -> a -> b)
        -> (r -> r -> b)
        -> (Func -> Func -> b)
        -> (b -> b -> b)
        -> ExprF a r
        -> ExprF a r
        -> b
zipExpr d n r f c = i where
  i (LitF a   ) (LitF b   ) = n a b
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
  NumExpr { _getNumExpr :: Expr a
          } deriving (Eq, Ord)

coerceNumBi :: (Expr a -> Expr a -> ExprF a (Expr a))
            -> NumExpr a -> NumExpr a -> NumExpr a
coerceNumBi = coerce

coerceNumUn :: (Expr a -> ExprF a (Expr a)) -> NumExpr a -> NumExpr a
coerceNumUn = coerce

instance Num a => Num (NumExpr a) where
  (+) = coerceNumBi AddF
  (*) = coerceNumBi MulF
  abs = coerceNumUn AbsF
  signum = coerceNumUn SigF
  negate = coerceNumUn NegF
  fromInteger = NumExpr . Fix . LitF . fromInteger

newtype IntExpr a =
  IntExpr { _getIntExpr :: Expr a
          } deriving (Eq, Ord)

coerceIntBi :: (Expr a -> Expr a -> ExprF a (Expr a))
            -> IntExpr a -> IntExpr a -> IntExpr a
coerceIntBi = coerce

coerceIntUn :: (Expr a -> ExprF a (Expr a)) -> IntExpr a -> IntExpr a
coerceIntUn = coerce

-- eval :: ExprType e => e a -> a
-- eval = cata evalAlg . view exprIso

instance Num a => Num (IntExpr a) where
  (+) = coerceIntBi AddF
  (*) = coerceIntBi MulF
  abs = coerceIntUn AbsF
  signum = coerceIntUn SigF
  negate = coerceIntUn NegF
  fromInteger = IntExpr . Fix . LitF . fromInteger

instance Real a => Real (IntExpr a) where
  toRational = toRational . cata evalAlg

instance Enum a => Enum (IntExpr a) where
  toEnum = IntExpr . Fix . LitF . toEnum
  fromEnum = fromEnum . cata evalAlg

instance Integral a => Integral (IntExpr a) where
  toInteger = toInteger . cata evalAlg
  quotRem a b = (coerceIntBi QutF a b, coerceIntBi RemF a b)
  quot = coerceIntBi QutF
  rem  = coerceIntBi RemF

newtype FracExpr a =
  FracExpr { _getFracExpr :: Expr a
           } deriving (Eq, Ord)

coerceFracBi :: (Expr a -> Expr a -> ExprF a (Expr a))
             -> FracExpr a -> FracExpr a -> FracExpr a
coerceFracBi = coerce

coerceFracUn :: (Expr a -> ExprF a (Expr a)) -> FracExpr a -> FracExpr a
coerceFracUn = coerce

instance Num a => Num (FracExpr a) where
  (+) = coerceFracBi AddF
  (*) = coerceFracBi MulF
  abs = coerceFracUn AbsF
  signum = coerceFracUn SigF
  negate = coerceFracUn NegF
  fromInteger = FracExpr . Fix . LitF . fromInteger

instance Fractional a => Fractional (FracExpr a) where
  fromRational = FracExpr . Fix . LitF . fromRational
  (/) = coerceFracBi DivF

newtype FloatExpr a =
  FloatExpr { _getFloatExpr :: Expr a
            } deriving (Eq, Ord)

coerceFloatBi :: (Expr a -> Expr a -> ExprF a (Expr a))
              -> FloatExpr a -> FloatExpr a -> FloatExpr a
coerceFloatBi = coerce

coerceFloatUn :: (Expr a -> ExprF a (Expr a)) -> FloatExpr a -> FloatExpr a
coerceFloatUn = coerce

instance Num a => Num (FloatExpr a) where
  (+) = coerceFloatBi AddF
  (*) = coerceFloatBi MulF
  abs = coerceFloatUn AbsF
  signum = coerceFloatUn SigF
  negate = coerceFloatUn NegF
  fromInteger = FloatExpr . Fix . LitF . fromInteger

instance Fractional a => Fractional (FloatExpr a) where
  fromRational = FloatExpr . Fix . LitF . fromRational
  (/) = coerceFloatBi DivF

instance Floating a => Floating (FloatExpr a) where
  pi    = (FloatExpr . Fix . LitF) pi
  exp   = coerceFloatUn $ AppF Exp
  log   = coerceFloatUn $ AppF Log
  sin   = coerceFloatUn $ AppF Sin
  cos   = coerceFloatUn $ AppF Cos
  asin  = coerceFloatUn $ AppF Asn
  acos  = coerceFloatUn $ AppF Acs
  atan  = coerceFloatUn $ AppF Atn
  sinh  = coerceFloatUn $ AppF Snh
  cosh  = coerceFloatUn $ AppF Csh
  asinh = coerceFloatUn $ AppF Ash
  acosh = coerceFloatUn $ AppF Ach
  atanh = coerceFloatUn $ AppF Ath

type instance Base (NumExpr   a) = ExprF a
type instance Base (IntExpr   a) = ExprF a
type instance Base (FracExpr  a) = ExprF a
type instance Base (FloatExpr a) = ExprF a

cproject :: Coercible (Expr a) (e a) => e a -> ExprF a (e a)
cproject = crce project where
  crce :: Coercible (Expr a) (e a) => (Expr a -> ExprF a (Expr a)) -> e a -> ExprF a (e a)
  crce = coerce

instance Functor.Foldable (NumExpr   a) where project = cproject
instance Functor.Foldable (IntExpr   a) where project = cproject
instance Functor.Foldable (FracExpr  a) where project = cproject
instance Functor.Foldable (FloatExpr a) where project = cproject

cembed :: Coercible (Expr a) (e a) => ExprF a (e a) -> e a
cembed = crce embed where
  crce :: Coercible (Expr a) (e a) => (ExprF a (Expr a) -> Expr a) -> ExprF a (e a) -> e a
  crce = coerce

instance Unfoldable (NumExpr   a) where embed = cembed
instance Unfoldable (IntExpr   a) where embed = cembed
instance Unfoldable (FracExpr  a) where embed = cembed
instance Unfoldable (FloatExpr a) where embed = cembed

platef :: (Unfoldable f, Functor.Foldable f, Traversable (Base f), Applicative m)
       => (f -> m f) -> f -> m f
platef f = fmap embed . traverse f . project

instance Plated (NumExpr   a) where plate = platef
instance Plated (IntExpr   a) where plate = platef
instance Plated (FracExpr  a) where plate = platef
instance Plated (FloatExpr a) where plate = platef
instance Traversable f => Plated (Fix f) where plate = platef

-- | A monadic anamorphism
anaM
  :: (Unfoldable t, Traversable (Base t), Monad m)
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

instance (Num a, Serialize a) => Serialize (NumExpr a) where
  put = cata putAlg
  get = anaM (const $ alg =<< getWord8) () where
    alg = \case
      0 -> LitF <$> get
      1 -> pure $ AbsF ()
      2 -> pure $ SigF ()
      6 -> pure $ NegF ()
      8 -> pure $ MulF () ()
      9 -> pure $ AddF () ()
      _ -> error "corrupted binary"

instance (Integral a, Serialize a) => Serialize (IntExpr a) where
  put = cata putAlg
  get = anaM (const $ alg =<< getWord8) () where
    alg = \case
      0 -> LitF <$> get
      1 -> pure $ AbsF ()
      2 -> pure $ SigF ()
      3 -> pure $ QutF () ()
      4 -> pure $ RemF () ()
      6 -> pure $ NegF ()
      8 -> pure $ MulF () ()
      9 -> pure $ AddF () ()
      _ -> error "corrupted binary"

instance (Fractional a, Serialize a) => Serialize (FracExpr a) where
  put = cata putAlg
  get = anaM (const $ alg =<< getWord8) () where
    alg = \case
      0 -> LitF <$> get
      1 -> pure $ AbsF ()
      2 -> pure $ SigF ()
      6 -> pure $ NegF ()
      7 -> pure $ DivF () ()
      8 -> pure $ MulF () ()
      9 -> pure $ AddF () ()
      _ -> error "corrupted binary"

instance (Floating a, Serialize a) => Serialize (FloatExpr a) where
  put = cata putAlg
  get = anaM (const $ alg =<< getWord8) () where
    alg = \case
      0 -> LitF <$> get
      1 -> pure $ AbsF ()
      2 -> pure $ SigF ()
      5 -> AppF <$> get ?? ()
      6 -> pure $ NegF ()
      7 -> pure $ DivF () ()
      8 -> pure $ MulF () ()
      9 -> pure $ AddF () ()
      _ -> error "corrupted binary"

pprAlg :: Show a => ExprF a (Int, ShowS) -> ShowS
pprAlg e = case e of
  LitF a        -> shows a
  NegF x       -> showString "-" . parR x
  AddF x y     -> parL x . showString " + " . parL y
  DivF x y     -> parL x . showString " / " . parR y
  MulF x y     -> parL x . showString " * " . parL y
  AppF f (_,x) -> shows f . showChar '(' . x . showChar ')'
  AbsF   (_,x) -> showString "abs(" . x . showChar ')'
  SigF   (_,x) -> showString "signum(" . x . showChar ')'
  QutF x y     -> parL x . showString " // " . parR y
  RemF x y     -> parL x . showString " % " . parR y
  where
    parL (c,p) = showParen (prec e >  c) p
    parR (c,p) = showParen (prec e >= c) p

instance Show a => Show (NumExpr   a) where showsPrec _ = zygo prec pprAlg
instance Show a => Show (IntExpr   a) where showsPrec _ = zygo prec pprAlg
instance Show a => Show (FracExpr  a) where showsPrec _ = zygo prec pprAlg
instance Show a => Show (FloatExpr a) where showsPrec _ = zygo prec pprAlg

pattern x :+: y = Fix (AddF x y)
pattern x :*: y = Fix (MulF x y)
pattern x :/: y = Fix (DivF x y)
pattern x :%: y = Fix (RemF x y)
pattern x :$: y = Fix (AppF x y)
pattern x :-: y = x :+: Neg y
pattern x :^: y = Exp :$: ((Log :$: x) :*: y)
pattern x ://: y = Fix (QutF x y)
pattern Neg x    = Fix (NegF x)
pattern Abs x    = Fix (AbsF x)
pattern Lit a    = Fix (LitF a)

class ExprType e where exprIso :: Iso' (Expr a) (e a)

instance ExprType NumExpr   where exprIso = coerced
instance ExprType IntExpr   where exprIso = coerced
instance ExprType FracExpr  where exprIso = coerced
instance ExprType FloatExpr where exprIso = coerced

assoc :: ExprType e => e a -> e a
assoc = under exprIso . rewrite $ \case
    x :+: (y :+: z) -> Just $ (x :+: y) :+: z
    x :*: (y :*: z) -> Just $ (x :*: y) :*: z
    _ -> Nothing

simplify :: (Num a, Eq a, ExprType e) => e a -> e a
simplify = under exprIso . rewrite $ \case
  x :+: Lit 0 -> Just x
  Lit 0 :+: x -> Just x
  x :/: Lit 1 -> Just x
  Lit 1 :*: x -> Just x
  x :*: Lit 1 -> Just x
  x :^: Lit 1 -> Just x
  Lit 1 :^: _ -> Just $ Lit 1
  _ :^: Lit 0 -> Just $ Lit 0
  Lit 0 :*: _ -> Just $ Lit 0
  _ :*: Lit 0 -> Just $ Lit 0
  _ :%: Lit 1 -> Just $ Lit 0
  Neg (Lit 0) -> Just $ Lit 0
  x :-:  y | x == y -> Just $ Lit 0
  x :/:  y | x == y -> Just $ Lit 1
  x :%:  y | x == y -> Just $ Lit 0
  x ://: y | x == y -> Just $ Lit 1
  _ -> Nothing
