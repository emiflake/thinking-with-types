{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module FirstClassFamilies () where

import Data.Maybe

class Eval l t | l -> t where
  eval :: l -> t

data Fst a b = Fst (a, b)

instance Eval (Fst a b) a where
  eval (Fst (a, _)) = a

-- Exercise 10.1-i
-- Defunctionalize `listToMaybe :: [a] -> Maybe a`
data ListToMaybe a = ListToMaybe [a]

instance Eval (ListToMaybe a) (Maybe a) where
  eval (ListToMaybe xs) = listToMaybe xs

