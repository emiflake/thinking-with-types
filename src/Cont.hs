{-# LANGUAGE ScopedTypeVariables #-}
module Cont () where

import           Iso ((:~=), iso)

cont :: a -> (forall r. (a -> r) -> r)
cont a = \callback -> callback a

runCont :: (forall r. (a -> r) -> r) -> a
runCont f = f id

newtype Cont a = Cont { unCont :: forall r. (a -> r) -> r }

contInOut :: a :~= Cont a
contInOut = iso to from
  where
    to val = Cont $ cont val
    from c = runCont $ unCont c

-- Exercise 6.4-i
-- Provide an `Functor` instance for `Cont`.
instance Functor Cont where
  fmap f (Cont c) = Cont ($ f (runCont c))

-- Exercise 6.4-ii
-- Provide an `Applicative` instance `Cont`.
instance Applicative Cont where
  pure a = Cont $ cont a
  (Cont cf) <*> (Cont ca) = Cont $ \k -> cf $ \f' -> ca $ \a' -> k (f' a')

-- Exercise 6.4-iii
-- Provide a `Monad` instance for `Cont`.
instance Monad Cont where
  return = pure
  (Cont ca) >>= f = Cont $ \cb -> ca $ \a -> unCont (f a) $ \b -> cb b
  -- This is quite ugly.

-- Exercise 6.4-iv
-- There is also a monad transformer version of `Cont`. Implement it.
-- TODO: do this
