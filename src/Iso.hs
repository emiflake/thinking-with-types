{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase     #-}
{-# LANGUAGE TypeOperators #-}
module Iso
  ( Iso(..)
  , iso
  , (:~=)
  ) where

import           Data.Tuple
import           Data.Void  (Void, absurd)

newtype Iso a b = Iso { unIso :: (a -> b, b -> a) }

type a :~= b = Iso a b

overIso :: ((a -> b, b -> a) -> c) -> Iso a b -> c
overIso = (. unIso)

iso :: (a -> b) -> (b -> a) -> Iso a b
iso to from = Iso (to, from)

sumUnitIso :: Either a Void :~= a
sumUnitIso = iso to from
  where
    to = \case
      Left a -> a
      Right v -> absurd v
    from = Left

isoPair :: (a, b) :~= (b, a)
isoPair = iso swap swap

isoSwap :: (Iso a b) :~= (Iso b a)
isoSwap = iso to from
  where
    to (Iso (to', from')) = Iso (from', to')
    from (Iso (from', to')) = Iso (to', from')

-- Exercise 1.2-i

{-
>  Determine the cardinality of
>
>   |Either Bool (Bool, Maybe Bool) -> Bool|
>   |Bool| ^ |Either Bool (Bool, Maybe Bool)|
>   2 ^ (|Bool| + |(Bool, Maybe Bool)|)
>   2 ^ (2 + |Bool| * |Maybe Bool|)
>   2 ^ (2 + 2 * (1 + |Bool|))
>   2 ^ (2 + 2 * (1 + 2))
>   2 ^ 8
>   256
-}

-- Exercise 1.4-i
-- Prove (a ^ b) ^ c ~= a ^ (b * c)
-- i.e. b -> c -> a ~= (b, c) -> a
curryUncurryIso :: (b -> c -> a) :~= ((b, c) -> a)
curryUncurryIso = iso to from
  where
    to = uncurry
    from = curry

-- Exercise 1.4-ii
-- Prove that a ^ b * a ^ c = a ^ (b + c)
-- i.e. (b -> a, c -> a) ~= Either b c -> a
bothSidesEitherIso :: (b -> a, c -> a) :~= (Either b c -> a)
bothSidesEitherIso = iso to from
  where
    to (ba, ca) = \case
      Left b -> ba b
      Right c -> ca c
    from f = (f . Left, f . Right)


-- Exercise 1.4-iii
-- Prove that (a * b) ^ c ~= a ^ c * b ^ c
-- i.e. (c -> (a, b)) ~= (c -> a, c -> b)
splitJoinFunctionIso :: (c -> (a, b)) :~= (c -> a, c -> b)
splitJoinFunctionIso = iso to from
  where
    to f = (fst . f, snd . f)
    from (ca, cb) = (,) <$> ca <*> cb

