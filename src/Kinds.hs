module Kinds (Not) where

-- Exercise 2.1.3-i
-- If `Show Int` has kind `Constraint`, what's the kind of `Show`
-- Answer:
-- Show :: Type -> Constraint
-- forall a. Show a :: Constraint

-- Exercise 2.1.3-ii
-- What is the kind of `Functor`
-- Answer:
-- Functor :: (Type -> Type) -> Constraint

-- Exercise 2.1.3-iii
-- What is the kind of `Monad`
-- Answer:
-- Monad :: (Type -> Type) -> Constraint

-- Exercise 2.1.3-iv
-- What is the kind of `MonadTrans`
-- Answer:
-- MonadTrans :: ((Type -> Type) -> Type -> Type) -> Constraint

-- Exercise 2.4-i
-- Write a closed type family to compute `Not`
type family Not (a :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True
