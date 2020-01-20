{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Roles () where

import           Data.Coerce   (Coercible (..), coerce)
import           Data.Foldable (toList)
import qualified Data.Map      as Map
import           Data.Monoid   (Product (..), Sum (..))

-- Exercise 8.2-i
-- What is the role signature of `Either a b`?
-- Answer:
--   `type role Either representational representational`

-- Exercise 8.2-ii
-- What is the role signature of `Proxy a`?
-- Answer:
--   `type role Proxy phantom`

