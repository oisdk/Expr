{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}

module ALC where

import           Data.Functor.Foldable
import           Data.Functor.Foldable.Extras

data (f :+: g) e = Inl (f e) | Inr (g e) deriving (Functor, Foldable, Traversable)

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f => f :<: f where
  inj = id

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj

foldCoprod :: (f e -> a) -> (g e -> a) -> (f :+: g) e -> a
foldCoprod l _ (Inl x) = l x
foldCoprod _ r (Inr x) = r x

inject :: (g :<: f ) => g (Fix f ) -> Fix f
inject = Fix . inj

data ExprF r =
  LitF Integer |
  AddF r r |
  SubF r r |
  MulF r r |
  AbsF r |
  SigF r |
  NegF r deriving Functor

instance (ExprF :<: f) => Num (Fix f) where
  x + y = inject (AddF x y)
  x - y = inject (SubF x y)
  x * y = inject (MulF x y)
  abs = inject . AbsF
  signum = inject . SigF
  fromInteger = inject . LitF
