module RankNTypes () where

-- Exercise 6.3-i
-- What is the rank of `Int -> forall a. a -> a`?
-- i.e. `Int -> (forall a. (a -> a))`
-- Answer: Rank-1

-- Exercise 6.3-ii
-- What is the rank of `(a -> b) -> (forall c. c -> a) -> b`?
-- i.e. `forall a b. (a -> b) -> (forall c. c -> a) -> b`
-- Answer: Rank-2

-- Exercise 6.3-iii
-- What is the rank of `((forall x. m x -> b (z m x)) -> b (z m a)) -> m a`
-- i.e. `forall m x a b. ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a`
-- Answer: Rank-3
