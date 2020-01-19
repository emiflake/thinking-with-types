{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Printf
  ( (:<<)
  , HasPrintf
  , format
  , Printf
  , printf
  ) where

import Data.Kind (Type)
import Data.Monoid ((<>))
import Data.Proxy (Proxy (..))
import GHC.TypeLits

data (a :: k1) :<< (b :: k2)
infixr 5 :<<


class HasPrintf a where
  type Printf a :: Type
  format :: String -> Proxy a -> Printf a

instance KnownSymbol text => HasPrintf (text :: Symbol) where
  type Printf text = String
  format s _ = s <> symbolVal (Proxy @text)


instance (HasPrintf a, KnownSymbol text) => HasPrintf ((text :: Symbol) :<< a) where
  type Printf (text :<< a) = Printf a
  format s _ = format (s <> symbolVal (Proxy @text)) (Proxy @a)

instance {-# OVERLAPPING #-} HasPrintf a => HasPrintf (String :<< a) where
  type Printf (String :<< a) = String -> Printf a
  format s _ = \p -> format (s <> p) (Proxy @a)

instance (HasPrintf a, Show t) => HasPrintf ((t :: Type) :<< a) where
  type Printf (t :<< a) = t -> Printf a
  format s _ = \p -> format (s <> show p) (Proxy @a)

printf :: HasPrintf a => Proxy a -> Printf a
printf = format ""
