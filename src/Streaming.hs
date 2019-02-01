-- Should have defined these functions the other way,
-- streamD to streamK.

module Streaming
    ( Stream
    , K.nil
    , K.cons
    , K.consM
    , K.maps
    , K.yield
    , K.replicate
    , K.replicateM
    , for
    , unfoldM
    , unfoldrM
    , run
    , concats
    , chunksOf
    , groupBy
    , group
    , intercalates
    , span
    , splitAt
    , append
    , enumFromStepN
    , drop
    , dropWhileM
    , dropWhile
    , drained
    , each
    , erase
    , filterM
    , filter
    , mapM
    , map
    , scanl'
    , take
    , takeWhileM
    , takeWhile
    , zipWithM
    , zipWith
    , foldlM'
    , foldl'
    , foldlM'_
    , foldl'_
    , foldrM
    , foldrM_
    , foldr
    , foldr_
    , mapM_
    , sum
    , sum_
    , toList
    , toList_
    , last
    , last_
    , effects
    , fromHandle
    , printStream
    , stdinLn
    , readLn
    , readFile
    , writeFile
    ) where

import Control.Monad.IO.Class
import Data.Functor.Identity
import Prelude
       hiding (drop, dropWhile, filter, foldr, last, map, mapM, mapM_,
               pred, readFile, readLn, span, splitAt, sum, take, takeWhile,
               writeFile, zipWith)
import System.IO hiding (readLn, readFile, writeFile)

import StreamingK (Stream, append)
import qualified StreamingD as D
import qualified StreamingK as K

import Of

{-# INLINE unfoldM #-}
unfoldM :: (Functor f, Monad m) => (s -> m (Either r (f s))) -> s -> Stream f m r
unfoldM gen seed = D.toStreamK $ D.unfoldM gen seed

{-# INLINE unfoldrM #-}
unfoldrM :: Monad m => (s -> m (Either r (a, s))) -> s -> Stream (Of a) m r
unfoldrM gen seed = D.toStreamK $ D.unfoldrM gen seed

{-# INLINE run #-}
run :: Monad m => Stream m m r -> m r
run = D.run . D.fromStreamK

{-# INLINE concats #-}
concats :: (Monad m, Functor f) => Stream (Stream f m) m r -> Stream f m r
concats m = D.toStreamK (D.concats (D.fromStreamK (K.maps D.fromStreamK m)))

{-# INLINE chunksOf #-}
chunksOf ::
       (Functor f, Monad m) => Int -> Stream f m r -> Stream (Stream f m) m r
chunksOf n = K.maps D.toStreamK . D.toStreamK . D.chunksOf n . D.fromStreamK

{-# INLINE intercalates #-}
intercalates :: (Functor f, Monad m) => Stream f m () -> Stream (Stream f m) m r -> Stream f m r
intercalates sep =
    D.toStreamK .
    D.intercalates (D.fromStreamK sep) . D.fromStreamK . K.maps D.fromStreamK

{-# INLINE span #-}
span ::
       Monad m
    => (a -> Bool)
    -> Stream (Of a) m r
    -> Stream (Of a) m (Stream (Of a) m r)
span pred = fmap D.toStreamK . D.toStreamK . D.span pred . D.fromStreamK

{-# INLINE groupBy #-}
groupBy ::
       Monad m
    => (a -> a -> Bool)
    -> Stream (Of a) m r
    -> Stream (Stream (Of a) m) m r
groupBy equals = K.maps D.toStreamK . D.toStreamK . D.groupBy equals . D.fromStreamK

{-# INLINE group #-}
group :: (Eq a, Monad m) => Stream (Of a) m r -> Stream (Stream (Of a) m) m r
group = groupBy (==)

{-# INLINE splitAt #-}
splitAt ::
       (Monad m, Functor f) => Int -> Stream f m r -> Stream f m (Stream f m r)
splitAt n = fmap D.toStreamK . D.toStreamK . D.splitAt n . D.fromStreamK

{-# INLINE enumFromStepN #-}
enumFromStepN :: (Num a, Monad m) => a -> a -> Int -> Stream (Of a) m ()
enumFromStepN x y = D.toStreamK . D.enumFromStepN x y

{-# INLINE take #-}
take :: (Functor f, Monad m) => Int -> Stream f m r -> Stream f m ()
take n = D.toStreamK . D.take n . D.fromStreamK

{-# INLINE takeWhileM #-}
takeWhileM :: Monad m => (a -> m Bool) -> Stream (Of a) m r -> Stream (Of a) m ()
takeWhileM pred = D.toStreamK . D.takeWhileM pred . D.fromStreamK

{-# INLINE takeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m ()
takeWhile f = takeWhileM (return . f)

{-# INLINE drop #-}
drop :: Monad m => Int -> Stream (Of a) m r -> Stream (Of a) m r
drop n = D.toStreamK . D.drop n . D.fromStreamK

{-# INLINE dropWhileM #-}
dropWhileM :: Monad m => (a -> m Bool) -> Stream (Of a) m r -> Stream (Of a) m ()
dropWhileM pred = D.toStreamK . D.dropWhileM pred . D.fromStreamK

{-# INLINE dropWhile #-}
dropWhile :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m ()
dropWhile f = dropWhileM (return . f)

{-# INLINE drained #-}
drained :: Monad m => Stream (Of a) m (Stream (Of b) m r) -> Stream (Of a) m r
drained = D.toStreamK . D.drained . D.fromStreamK . fmap D.fromStreamK

{-# INLINE each #-}
each :: Monad m => [a] -> Stream (Of a) m ()
each = D.toStreamK . D.each

{-# INLINE erase #-}
erase :: Monad m => Stream (Of a) m r -> Stream Identity m r
erase = D.toStreamK . D.erase . D.fromStreamK

{-# INLINE filterM #-}
filterM :: Monad m => (a -> m Bool) -> Stream (Of a) m r -> Stream (Of a) m r
filterM pred = D.toStreamK . D.filterM pred . D.fromStreamK

{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m r
filter f = filterM (return . f)

{-# INLINE mapM #-}
mapM :: Monad m => (a -> m b) -> Stream (Of a) m r -> Stream (Of b) m r
mapM f = D.toStreamK . D.mapM f . D.fromStreamK

{-# INLINE map #-}
map :: Monad m => (a -> b) -> Stream (Of a) m r -> Stream (Of b) m r
map f = mapM (return . f)

{-# INLINE mapM_ #-}
mapM_ :: Monad m => (a -> m b) -> Stream (Of a) m r -> m r
mapM_ f = D.mapM_ f . D.fromStreamK

{-# INLINE for #-}
for :: (Monad m, Functor f) => Stream (Of a) m r -> (a -> Stream f m x) -> Stream f m r
for m f = D.toStreamK $ D.for (D.fromStreamK m) (D.fromStreamK . f)

{-# INLINE scanl' #-}
scanl' :: Monad m => (b -> a -> b) -> b -> Stream (Of a) m r -> Stream (Of b) m r
scanl' fstep begin = D.toStreamK . D.scanl' fstep begin . D.fromStreamK

{-# INLINE zipWithM #-}
zipWithM ::
       Monad m
    => (a -> b -> m c)
    -> Stream (Of a) m r
    -> Stream (Of b) m r
    -> Stream (Of c) m r
zipWithM f m1 m2 = D.toStreamK (D.zipWithM f (D.fromStreamK m1) (D.fromStreamK m2))

{-# INLINE zipWith #-}
zipWith ::
       Monad m
    => (a -> b -> c)
    -> Stream (Of a) m r
    -> Stream (Of b) m r
    -> Stream (Of c) m r
zipWith f = zipWithM (\a b -> return (f a b))

{-# INLINE foldlM' #-}
foldlM' :: Monad m => (b -> a -> m b) -> b -> Stream (Of a) m r -> m (Of b r)
foldlM' fstep begin = D.foldlM' fstep begin . D.fromStreamK

{-# INLINE foldl' #-}
foldl' :: Monad m => (b -> a -> b) -> b -> Stream (Of a) m r -> m (Of b r)
foldl' f = foldlM' (\b a -> return $ f b a)

{-# INLINE foldlM'_ #-}
foldlM'_ :: Monad m => (b -> a -> m b) -> b -> Stream (Of a) m r -> m b
foldlM'_ fstep begin = D.foldlM'_ fstep begin . D.fromStreamK

{-# INLINE foldl'_ #-}
foldl'_ :: Monad m => (b -> a -> b) -> b -> Stream (Of a) m r -> m b
foldl'_ fstep = foldlM'_ (\b a -> return (fstep b a))

{-# INLINE foldrM #-}
foldrM :: Monad m => (a -> b -> m b) -> b -> Stream (Of a) m r -> m (Of b r)
foldrM fstep begin = D.foldrM fstep begin . D.fromStreamK

{-# INLINE foldr #-}
foldr :: Monad m => (a -> b -> b) -> b -> Stream (Of a) m r -> m (Of b r)
foldr f = foldrM (\a b -> return (f a b))

{-# INLINE foldrM_ #-}
foldrM_ :: Monad m => (a -> b -> m b) -> b -> Stream (Of a) m r -> m b
foldrM_ fstep begin = D.foldrM_ fstep begin . D.fromStreamK

{-# INLINE foldr_ #-}
foldr_ :: Monad m => (a -> b -> b) -> b -> Stream (Of a) m r -> m b
foldr_ f = foldrM_ (\a b -> return (f a b))

{-# INLINE last #-}
last :: Monad m => Stream (Of a) m r -> m (Of (Maybe a) r)
last = foldl' (\_ y -> Just y) Nothing

{-# INLINE last_ #-}
last_ :: Monad m => Stream (Of a) m r -> m (Maybe a)
last_ = foldl'_ (\_ y -> Just y) Nothing

{-# INLINE sum #-}
sum :: (Monad m, Num a) => Stream (Of a) m r -> m (Of a r)
sum = foldl' (+) 0

{-# INLINE sum_ #-}
sum_ :: (Monad m, Num a) => Stream (Of a) m r -> m a
sum_ = foldl'_ (+) 0

{-# INLINE toList #-}
toList :: Monad m => Stream (Of a) m r -> m (Of [a] r)
toList = D.toList . D.fromStreamK

{-# INLINE toList_ #-}
toList_ :: Monad m => Stream (Of a) m r -> m [a]
toList_ = D.toList_ . D.fromStreamK

{-# INLINE stdinLn #-}
stdinLn :: MonadIO m => Stream (Of String) m ()
stdinLn = D.toStreamK D.stdinLn

{-# INLINE readLn #-}
readLn :: (MonadIO m, Read a) => Stream (Of a) m ()
readLn = D.toStreamK D.readLn

{-# INLINE effects #-}
effects :: Monad m => Stream (Of a) m r -> m r
effects = D.effects . D.fromStreamK

{-# INLINE fromHandle #-}
fromHandle :: MonadIO m => Handle -> Stream (Of String) m ()
fromHandle h = D.toStreamK $ D.fromHandle h

{-# INLINE toHandle #-}
toHandle :: MonadIO m => Handle -> Stream (Of String) m r -> m r
toHandle h = D.toHandle h . D.fromStreamK

{-# INLINE printStream #-}
printStream :: (MonadIO m, Show a) => Stream (Of a) m r -> m r
printStream = D.printStream . D.fromStreamK

readFile :: FilePath -> (Stream (Of String) IO () -> IO a) -> IO a
readFile f s = withFile f ReadMode $ \h -> s (fromHandle h)

writeFile :: FilePath -> Stream (Of String) IO r -> IO r
writeFile f = withFile f WriteMode . flip toHandle
