module Variance () where

import           Data.Functor.Contravariant
import           Data.Functor.Invariant

newtype T1 a = T1 (Int -> a)
newtype T2 a = T2 (a -> Int)
newtype T3 a = T3 (a -> a)
newtype T4 a = T4 ((Int -> a) -> Int)
newtype T5 a = T5 ((a -> Int) -> Int)

-- Exercise 3-i
-- Which of these types are `Functor`s?
-- Give instances for the ones that are.

instance Functor T1 where
  fmap f (T1 g) = T1 $ f . g

instance Contravariant T2 where
  contramap f (T2 g) = T2 $ g . f

instance Invariant T3 where
  invmap f g (T3 h) = T3 $ f . h . g

instance Contravariant T4 where
  contramap f (T4 g) = T4 $ \h -> g (f . h)

instance Functor T5 where
  fmap f (T5 g) = T5 $ \h -> g (h . f)
