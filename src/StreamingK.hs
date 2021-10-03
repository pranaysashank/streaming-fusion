
-- from fuse
-- Using a clever trick from streamly where we convert between
-- CPS and Direct type. CPS style performs way better in
-- operations like append.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module StreamingK
    ( Stream(..)
    , append
    , cons
    , consM
    , nil
    , yield
    , maps
    , replicate
    , replicateM
    ) where

import Control.Monad (ap)
import Control.Monad.Trans.Class (MonadTrans(..))
import Prelude hiding (replicate)
import Of

newtype Stream f m r =
    Stream { unStream :: forall x.
              (f (Stream f m r) -> m x) -- yield
           -> (f r -> m x) -- singleton
           -> (r -> m x) -- return
           -> m x
           }

{-# INLINE nil #-}
nil :: r -> Stream f m r
nil r = Stream $ \_ _ stp -> stp r

{-# INLINE cons #-}
cons :: a -> Stream (Of a) m r -> Stream (Of a) m r
cons a m = Stream $ \yld _ _ -> yld (a :> m)

{-# INLINE consM #-}
consM :: Monad m => m a -> Stream (Of a) m r -> Stream (Of a) m r
consM m rest = Stream $ \yld _ _ -> m >>= \a -> yld (a :> rest)

{-# INLINE yield #-}
yield :: a -> Stream (Of a) m ()
yield a = Stream $ \_ sng _ -> sng (a :> ())

{-# INLINE maps #-}
maps :: Functor g => (forall x. f x -> g x) -> Stream f m r -> Stream g m r
maps phi m = go m
  where
    go m1 =
        Stream $ \yld sng stp ->
            let yieldk fs = yld (fmap go (phi fs))
                single fr = sng (phi fr)
            in unStream m1 yieldk single stp

{-# INLINE replicateM #-}
replicateM :: Monad m => Int -> m a -> Stream (Of a) m ()
replicateM n m = go n
  where
    go i =
        if i <= 0
            then nil ()
            else m `consM` go (i - 1)

{-# INLINE replicate #-}
replicate :: Int -> a -> Stream (Of a) m ()
replicate n a = go n
  where
    go i =
        if i <= 0
            then nil ()
            else a `cons` go (i - 1)

-- XXX If we constrain r to a Monoid, we can then mappend the r's together.
-- Maybe we can declare a Monoid instance, that has a monoid constraint on r,
-- we can then reuse standard prelude functions like foldMap.
{-# INLINE append #-}
append :: (Functor f) => Stream f m r -> Stream f m s -> Stream f m (r,s)
append m1 m2 = go m1
  where
    go m =
        Stream $ \yld sng stp ->
            let stop r = unStream (fmap (\s -> (r, s)) m2) yld sng stp
                single fr = yld (fmap (\r -> fmap (\s -> (r, s)) m2) fr)
                yieldk fs = yld (fmap go fs)
            in unStream m yieldk single stop

{-instance Functor f => Semigroup (Stream f m r) where
    (<>) = append-}

instance Functor f => Functor (Stream f m) where
    -- fmap :: (a -> b) -> Stream f m a -> Stream f m b
    {-# INLINE fmap #-}
    fmap f m1 = go m1
      where
        go m = Stream $ \yld sng stp ->
                   let stop r = stp (f r)
                       yieldk fs = yld (fmap go fs)
                       single fr = sng (fmap f fr)
                    in unStream m yieldk single stop

instance Functor f => Applicative (Stream f m) where
   {-# INLINE pure #-}
   pure = nil

   -- (<*>) :: Stream f m (a -> b) -> Stream f m a -> Stream f m c
   {-# INLINABLE (<*>) #-}
   (<*>) = ap

instance (Functor f) => Monad (Stream f m) where
    {-# INLINE return #-}
    return = pure
    -- (>>=) :: Stream f m r -> (r -> Stream f m r1) -> Stream f m r1
    {-# INLINABLE (>>=) #-}
    xs >>= f = Stream $ \yld sng stp ->
                   let stop r = unStream (f r) yld sng stp
                       yieldk fs = unStream (consL (fmap (>>= f) fs)) yld sng stp
                       single fr = unStream (consL (fmap f fr)) yld sng stp
                   in unStream xs yieldk single stop

instance MonadTrans (Stream f) where
    {-# INLINE lift #-}
    lift m = go (fmap nil m)
        where
        go mstrm = Stream $ \yld sng r ->
             mstrm >>= \strm -> unStream strm yld sng r

{-# INLINE consL #-}
consL :: f (Stream f m r) -> Stream f m r
consL xs = Stream (\yld _ _ -> yld $! xs)
