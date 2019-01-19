{-# LANGUAGE DeriveGeneric #-}

module Of
    ( Of(..)
    ) where

import GHC.Generics

data Of a b = !a :> b deriving (Generic)

instance Functor (Of a) where
    {-# INLINE fmap #-}
    fmap f (a :> x) = a :> (f x)
