{-# LANGUAGE DeriveGeneric #-}

module Of
    ( Of(..)
    ) where

import GHC.Generics

data Of a b = !a :> b deriving (Show, Generic)

instance Functor (Of a) where
    {-# INLINE fmap #-}
    fmap f (a :> x) = a :> (f x)
