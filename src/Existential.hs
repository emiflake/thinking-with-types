{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
module Existential
  ( Any(..)
  , Has(..)
  , Dynamic
  , liftD2
  , fromDynamic
  , dynPlus
  ) where

import           Data.Foldable
import           Data.Maybe
import           Data.Typeable
import           GHC.Types     hiding (Any)


data Any where
  Any :: a -> Any

elimAny :: (forall a. a -> r) -> Any -> r
elimAny f (Any a) = f a

-- Exercise 7.1-i
-- Are functions of type `forall a. a -> r` interesting?
-- Answer: no, they must be `forall a. const a`, so they
-- are essentially values.
-- e.g.
five = elimAny (const 5) (Any "hello")
onlyFives = fmap (elimAny (const 5)) $ [Any "hi", Any 5]
-- The reason is, we are polymorphic over type a, so we cannot
-- inspect anything about the type. In essence, we have to ignore
-- the value we get in.

data Has (c :: Type -> Constraint) where
  Has :: c a => a -> Has c

-- Now all the sudden this is useful.
-- Exercise 7.1-ii
-- What happens to this instance if you remove the `Show t =>` constraint from `HasShow`?
-- Answer: We are no longer allowed to call `show`
-- as GHC is unable to find out which instance of `Show` to use.

elimHasShow :: (forall a. Show a => a -> r) -> Has Show -> r
elimHasShow f (Has a) = f a

-- Exercise 7.1-iii
-- Write the `Show` instance for `HasShow` in terms of `elimHasShow`.
instance Show (Has Show) where
  show hasShow = "HasShow " <> elimHasShow show hasShow

type Dynamic = Has Typeable

elimDynamic :: (forall a. Typeable a => a -> r) -> Dynamic  -> r
elimDynamic f (Has a) = f a

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

liftD2 :: forall a b r. ( Typeable a
                        , Typeable b
                        , Typeable r
                        )
       => Dynamic
       -> Dynamic
       -> (a -> b -> r)
       -> Maybe Dynamic
liftD2 d1 d2 f =
  fmap Has . f <$> fromDynamic @a d1
               <*> fromDynamic @b d2

dynPlus :: Dynamic -> Dynamic -> Dynamic
dynPlus a b =
  fromMaybe (error "bad types for dynPlus") $ asum
    [ liftD2 @String @String a b (++)
    , liftD2 @Int    @Int    a b (+)
    ]
