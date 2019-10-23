-- Not every function has been tested to work in this module.
-- Monoid instance is trivial but not implemented.
-- Function signatures match the original streaming library,
-- not all functions have been ported yet.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

#include "inline.hs"

module StreamingD
    ( Stream (..)
    , Step (..)
    , toStreamK
    , fromStreamK
    , for
    , unfoldM
    , unfoldrM
    , mapsM
    , maps
    , run
    , store
    , copy
    , concats
    , chunksOf
    , groupBy
    , group
    , intercalates
    , span
    , splitAt
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
    , inspect
    , foldlM'
    , foldl'
    , foldlM'_
    , foldl'_
    , foldrM
    , foldrM_
    , mapM_
    , sum
    , sum_
    , toList
    , toList_
    , yield
    , last
    , last_
    , effects
    , fromHandle
    , toHandle
    , printStream
    , stdinLn
    , readLn
    ) where

import Control.Monad.IO.Class
import Data.Functor.Identity (Identity(..))
import Prelude
       hiding (drop, dropWhile, filter, last, map, mapM, mapM_, pred,
               readLn, span, splitAt, sum, take, takeWhile, zipWith)
import GHC.Types (SPEC(..))
import System.IO hiding (readLn)
import Text.Read (readMaybe)

import qualified StreamingK as K
import Of

data Stream f m r = forall s. Stream (s -> m (Step f s r)) s

data Step f s r = Yield !(f s)
                | Skip s
                | Return r

instance Monad m => Functor (Stream f m) where
  -- fmap :: (a -> b) -> Stream f m a -> Stream f m b
  {-# INLINABLE fmap #-}
  fmap f (Stream step state) = Stream step' state
    where
      step' st = do
          nxt <- step st
          return $ case nxt of
              Yield fs -> Yield fs
              Skip   s -> Skip s
              Return r -> Return (f r)

instance (Functor f, Monad m) => Applicative (Stream f m) where
    {-# INLINE pure #-}
    pure r = Stream (\_ -> return $ Return r) ()

--  (<*>) :: Stream f m (a -> b) -> Stream f m a -> Stream f m b
    {-# INLINE (<*>) #-}
    (Stream step state) <*> (Stream istep istate) = Stream step' (Left state)
      where
        step' (Left st) = do
            nxt <- step st
            return $ case nxt of
                Yield fs -> Yield (Left `fmap` fs)
                Skip   s -> Skip (Left s)
                Return r -> Skip (Right (istate, r))

        step' (Right (ist, f)) = do
            nxt <- istep ist
            return $ case nxt of
                Yield fs -> Yield (fmap (\s -> Right (s, f)) fs)
                Skip   s -> Skip (Right (s, f))
                Return r -> Return (f r)

instance (Functor f, Monad m) => Monad (Stream f m) where
    {-# INLINE return #-}
    return =
        pure

--  (>>=) :: Stream f m a -> (a -> Stream f m b) -> Stream f m b
    {-# INLINABLE (>>=) #-}
    (Stream step state) >>= f = Stream step' (Left state)
       where
         step' (Left st) = do
             nxt <- step st
             return $ case nxt of
                 Yield fs -> Yield (Left `fmap` fs)
                 Skip   s -> Skip (Left s)
                 Return r -> Skip (Right $ f r)

         step' (Right (Stream istep istate)) = do
              nxt <- istep istate
              return $ case nxt of
                  Yield fs -> Yield ((Right . (Stream istep)) `fmap` fs)
                  Skip   s -> Skip (Right (Stream istep s))
                  Return r -> Return r

instance (Functor f, MonadIO m) => MonadIO (Stream f m) where
    liftIO m = Stream (\_ -> liftIO m >>= return . Return) ()

{-# INLINE_NORMAL cons #-}
cons :: Monad m => a -> Stream (Of a) m r -> Stream (Of a) m r
cons a (Stream step state) = Stream step' Nothing
  where
    {-# INLINE_LATE step' #-}
    step' Nothing = return $ Yield (a :> Just state)
    step' (Just st) = do
        nxt <- step st
        return $ case nxt of
            Yield (x :> rst) -> Yield (x :> Just rst)
            Skip s -> Skip (Just s)
            Return r -> Return r

{-# INLINE_NORMAL inspect #-}
inspect :: (Functor f, Monad m) => Stream f m r -> m (Either r (f (Stream f m r)))
inspect (Stream step state) = go state
  where
    go st = do
        nxt <- step st
        case nxt of
            Yield fs -> return (Right (fmap (Stream step) fs))
            Skip s -> go s
            Return r -> return (Left r)

{-# INLINE_NORMAL unfoldM #-}
unfoldM :: Monad m => (s -> m (Either r (f s))) -> s -> Stream f m r
unfoldM gen state = (Stream step state)
  where
    {-# INLINE_LATE step #-}
    step st = do
        nxt <- gen st
        case nxt of
            Left r -> return $ Return r
            Right fs -> return $ Yield fs

{-# INLINE_NORMAL unfoldrM #-}
unfoldrM :: Monad m => (s -> m (Either r (a, s))) -> s -> Stream (Of a) m r
unfoldrM gen state = Stream step state
  where
    {-# INLINE_LATE step #-}
    step st = do
        nxt <- gen st
        case nxt of
            Left r -> return $ Return r
            Right (x, s) -> return $ Yield (x :> s)

-- XXX This doesn't have a Functor constraint, is this right?
{-# INLINE_NORMAL maps #-}
maps ::
       Monad m
    => (forall x. f x -> g x)
    -> Stream f m r
    -> Stream g m r
maps natTrans (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' st = do
        nxt <- step st
        return $
            case nxt of
                Yield fs -> Yield (natTrans fs)
                Skip s -> Skip s
                Return r -> Return r

{-# INLINE_NORMAL mapsM #-}
mapsM :: Monad m => (forall x. f x -> m (g x)) -> Stream f m r -> Stream g m r
mapsM phi (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' st = do
        nxt <- step st
        case nxt of
            Yield fs -> do
              gs <- phi fs
              return $ Yield gs
            Skip s -> return $ Skip s
            Return r -> return $ Return r

{-# INLINABLE run #-}
run :: Monad m => Stream m m r -> m r
run (Stream step state) = go SPEC state
  where
    go !_ st = do
        nxt <- step st
        case nxt of
            Yield fs -> fs >>= go SPEC
            Skip s -> go SPEC s
            Return r -> return r

lift :: Monad m => m a -> Stream f m a
lift m = Stream (\_ -> m >>= return . Return) ()

{-# INLINE store #-}
store :: Monad m => (Stream (Of a) (Stream (Of a) m) r -> t) -> Stream (Of a) m r -> t
store f x = f (copy x)

copy :: Monad m => Stream (Of a) m r -> Stream (Of a) (Stream (Of a) m) r
copy (Stream step state) = Stream step' state
  where
    step' st = do
        nxt <- lift (step st)
        case nxt of
            Yield (a :> s) -> Stream istep (Left a, s)
            Skip s -> Stream (\_ -> return $ Return (Skip s)) ()
            Return r -> Stream (\_ -> return $ Return (Return r)) ()
    istep (Left prev, st) = return $ Yield (prev :> (Right prev, st))
    istep (Right prev, st) = return $ Return (Yield (prev :> st))

{-# INLINE_NORMAL concats #-}
concats :: (Monad m, Functor f) => Stream (Stream f m) m r -> Stream f m r
concats (Stream step state) = Stream step' (Left state)
  where
    {-# INLINE_LATE step' #-}
    step' (Left st) = do
        nxt <- step st
        return $ case nxt of
            Yield fs -> Skip (Right fs)
            Skip s -> Skip (Left s)
            Return r -> Return r
    step' (Right (Stream istep ist)) = do
        nxt <- istep ist
        return $
            case nxt of
                Yield fs -> Yield (fmap (\s -> Right (Stream istep s)) fs)
                Skip s -> Skip (Right (Stream istep s))
                Return r -> Skip (Left r)

-- XXX Add tests.
--- chunksOf 0 == chunksOf 1. This is consistent with the
-- streaming library's behaviour.
{-# INLINE_NORMAL chunksOf #-}
chunksOf ::
       (Functor f, Monad m) => Int -> Stream f m r -> Stream (Stream f m) m r
chunksOf n (Stream step state) = n `seq` Stream step' (X state)
  where
    {-# INLINE_LATE step' #-}
    step' (X st) = do
        nxt <- step st
        return $
            case nxt of
                Yield fs -> Yield (Stream ostep (Left (fs, 0)))
                Skip s -> Skip (X s)
                Return r -> Return r
    step' (Y r) = return $ Return r
    step' (Z fs) = return $ Yield (Stream ostep (Left (fs, 0)))

    {-# INLINE_LATE ostep #-}
    ostep (Left (fs, i)) = i `seq`
        return $ Yield ((\s -> Right (s, i + 1)) `fmap` fs)
    ostep (Right (st, i)) = i `seq` do
        nxt <- step st
        return $
            case nxt of
                Yield fs ->
                    if i < n
                        then Yield ((\s -> Right (s, i + 1)) `fmap` fs)
                        else Return (Z fs)
                Skip s -> Skip (Right (s, i))
                Return r -> Return (Y r)

data A x y z = X x | Y y | Z z

{-# INLINE_NORMAL intercalates #-}
intercalates :: (Functor f, Monad m) => Stream f m () -> Stream (Stream f m) m r -> Stream f m r
intercalates sep (Stream step state) = Stream step' (X state)
  where
    {-# INLINE_LATE step' #-}
    step' (X st) = do
        nxt <- step st
        return $ case nxt of
            Yield fs -> Skip (Y fs)
            Skip s -> Skip (X s)
            Return r -> Return r

    step' (Y (Stream istep ist)) = do
        nxt <- istep ist
        return $
            case nxt of
                Yield fs -> Yield (fmap (\s -> Y (Stream istep s)) fs)
                Skip s -> Skip (Y (Stream istep s))
                Return r -> Skip (Z (sep, r))

    step' (Z (Stream istep ist, r)) = do
        nxt <- istep ist
        return $
            case nxt of
                Yield fs -> Yield (fmap (\s -> Z (Stream istep s, r)) fs)
                Skip s -> Skip (Z (Stream istep s, r))
                Return _ -> Skip (X r)

-- XXX Implement without cons.
{-# INLINE_NORMAL span #-}
span ::
       Monad m
    => (a -> Bool)
    -> Stream (Of a) m r
    -> Stream (Of a) m (Stream (Of a) m r)
span pred (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' st = do
        nxt <- step st
        return $
            case nxt of
                Yield (a :> rst) ->
                    if pred a
                        then Yield (a :> rst)
                        else Return (a `cons` (Stream step rst))
                Skip s -> Skip s
                Return r -> Return (Stream (\_ -> return $ Return r) ())

-- XXX Implement without cons.
-- XXX Can't believe this actually works.
-- >>> S.printStream $ mapsM S.toList $ S.groupBy (>=) $ each [1,2,3,1,2,3,4,3,2,4,5,6,7,6,5]
{-# INLINE_NORMAL groupBy #-}
groupBy ::
       Monad m
    => (a -> a -> Bool)
    -> Stream (Of a) m r
    -> Stream (Stream (Of a) m) m r
groupBy equals (Stream step state) = Stream step' (Right (state, Nothing))
  where
    {-# INLINE_LATE step' #-}
    step' (Right (st, Nothing)) = do
        nxt <- step st
        return $
            case nxt of
                Yield (a :> rst) -> Yield (a `cons` Stream ostep (rst, a))
                Skip s -> Skip (Right (s, Nothing))
                Return r -> Return r
    step' (Right (st, Just a)) = return $ Yield (a `cons` Stream ostep (st, a))
    step' (Left r) = return $ Return r

    {-# INLINE_LATE ostep #-}
    ostep (st, a) = do
        nxt <- step st
        return $
            case nxt of
                Yield (x :> rst) ->
                    if equals a x
                        then Yield (x :> (rst, a))
                        else Return (Right (rst, Just x))
                Skip s -> Skip (s, a)
                Return r -> Return (Left r)

{-# INLINE group #-}
group :: (Eq a, Monad m) => Stream (Of a) m r -> Stream (Stream (Of a) m) m r
group = groupBy (==)

-- XXX This looks wrong. Could this be made simpler?
{-# INLINE_NORMAL splitAt #-}
splitAt ::
       (Monad m, Functor f) => Int -> Stream f m r -> Stream f m (Stream f m r)
splitAt n str
    | n <= 0 = (Stream (\_ -> return $ Return str) ())
splitAt n (Stream step state) = Stream step' (Left (state, n-1))
  where
    {-# INLINE_LATE step' #-}
    step' (Left (st, i)) = do
        nxt <- step st
        return $
            case nxt of
                Yield fs ->
                    if i == 0
                        then Yield (fmap Right fs)
                        else Yield (fmap (\s -> Left (s, i-1)) fs)
                Skip   s -> Skip $ Left (s, i)
                Return r -> Return (Stream (\_ -> return (Return r)) ())
    step' (Right s) = return $ Return (Stream step s)

-- TODO: Provide an enumerable instance instead of
-- whatever vector was trying to do.
{-# INLINE_NORMAL enumFromStepN #-}
enumFromStepN :: (Num a, Monad m) => a -> a -> Int -> Stream (Of a) m ()
enumFromStepN x y n = x `seq` y `seq` n `seq` Stream step (x, n)
  where
    {-# INLINE_LATE step #-}
    step (w, m) | m > 0 = return $ Yield (w :> (w+y,m))
                | otherwise = return $ Return ()

{-# INLINE_NORMAL take #-}
{-# SPECIALIZE take :: Monad m => Int -> Stream (Of a) m r -> Stream (Of a) m () #-}
take :: (Functor f, Monad m) => Int -> Stream f m r -> Stream f m ()
take n (Stream step state) = n `seq` Stream step' (state, 0)
  where
    {-# INLINE_LATE step' #-}
    step' (st, i) | i < n = do
        nxt <- step st
        return $ case nxt of
            Yield fs -> Yield (fmap (\s -> (s, i+1)) fs)
            Skip  s  -> Skip (s, i)
            Return _ -> Return ()
    step' _ = return $ Return ()

{-# INLINE_NORMAL takeWhileM #-}
takeWhileM :: Monad m => (a -> m Bool) -> Stream (Of a) m r -> Stream (Of a) m ()
takeWhileM f (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' st = do
        nxt <- step st
        case nxt of
            Yield (x :> rst) -> do
                b <- f x
                return $
                    if b
                        then Yield (x :> rst)
                        else Return ()
            Skip s -> return $ Skip s
            Return _ -> return $ Return ()

{-# INLINE takeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m ()
takeWhile f = takeWhileM (return . f)

{-# INLINE_NORMAL drop #-}
drop :: Monad m => Int -> Stream (Of a) m r -> Stream (Of a) m r
drop n str
    | n <= 0 = str
drop n (Stream step state) = (Stream step' (state, 0))
  where
    {-# INLINE_LATE step' #-}
    step' (st, i)
        | i < n = do
            nxt <- step st
            return $
                case nxt of
                    Yield (_ :> rest) -> Skip (rest, i + 1)
                    Skip s -> Skip (s, i)
                    Return r -> Return r
        | otherwise = do
            nxt <- step st
            return $
                case nxt of
                    Yield (x :> rest) -> Yield (x :> (rest, i))
                    Skip s -> Skip (s, i)
                    Return r -> Return r

{-# INLINE_NORMAL dropWhileM #-}
dropWhileM :: Monad m => (a -> m Bool) -> Stream (Of a) m r -> Stream (Of a) m ()
dropWhileM f (Stream step state) = Stream step' (Left state)
  where
    {-# INLINE_LATE step' #-}
    step' (Left st) = do
        nxt <- step st
        case nxt of
            Yield (x :> rst) -> do
                b <- f x
                return $ if b then Skip (Left rst) else Yield (x :> Right rst)
            Skip s -> return $ Skip (Left s)
            Return _ -> return $ Return ()

    step' (Right st) = do
        nxt <- step st
        return $ case nxt of
            Yield (x :> rst) -> Yield (x :> Right rst)
            Skip s           -> Skip (Right s)
            Return _         -> Return ()

{-# INLINE dropWhile #-}
dropWhile :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m ()
dropWhile f = dropWhileM (return . f)

{-# INLINE_NORMAL drained #-}
drained :: Monad m => Stream (Of a) m (Stream (Of b) m r) -> Stream (Of a) m r
drained (Stream step state) = Stream step' (Left state)
  where
    {-# INLINE_LATE step' #-}
    step' (Left st) = do
        nxt <- step st
        return $ case nxt of
            Yield (a :> rest) -> Yield (a :> (Left rest))
            Skip s -> Skip (Left s)
            Return r -> Skip (Right r)
    step' (Right (Stream rstep rst)) = do
        nxt <- rstep rst
        return $ case nxt of
            Yield (_ :> rest) -> Skip (Right (Stream rstep rest))
            Skip s -> Skip (Right (Stream rstep s))
            Return r -> Return r

{-# INLINE_NORMAL each #-}
each :: Monad m => [a] -> Stream (Of a) m ()
each = Stream step
  where
    {-# INLINE_LATE step #-}
    step (x:xs) = return $ Yield (x :> xs)
    step _ = return $ Return ()

{-# INLINE_NORMAL erase #-}
erase :: Monad m => Stream (Of a) m r -> Stream Identity m r
erase (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' st = do
        nxt <- step st
        return $ case nxt of
            Yield (_ :> rest) -> Yield (Identity rest)
            Skip s -> Skip s
            Return r -> Return r

{-# INLINE_NORMAL filterM #-}
filterM :: Monad m => (a -> m Bool) -> Stream (Of a) m r -> Stream (Of a) m r
filterM pred (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' st = do
        nxt <- step st
        case nxt of
            Yield (x :> rest) -> do
                b <- pred x
                return $
                    if b
                        then Yield (x :> rest)
                        else Skip rest
            Skip s -> return $ Skip s
            Return r -> return $ Return r

{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m r
filter f = filterM (return . f)

{-# INLINE_NORMAL mapM #-}
mapM :: Monad m => (a -> m b) -> Stream (Of a) m r -> Stream (Of b) m r
mapM f (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' st = do
        nxt <- step st
        case nxt of
            Yield (x :> rest) -> f x >>= \y -> return $ Yield (y :> rest)
            Skip s -> return $ Skip s
            Return r -> return $ Return r

{-# INLINE map #-}
map :: Monad m => (a -> b) -> Stream (Of a) m r -> Stream (Of b) m r
map f = mapM (return . f)

{-# INLINE_NORMAL mapM_ #-}
mapM_ :: Monad m => (a -> m b) -> Stream (Of a) m r -> m r
mapM_ f (Stream step state) = go SPEC state
  where
    go !_ st = do
        nxt <- step st
        case nxt of
            Yield (x :> rest) -> f x *> go SPEC rest
            Skip s -> go SPEC s
            Return r -> return r

{-# INLINE_NORMAL for #-}
for :: (Functor f, Monad m) => Stream (Of a) m r -> (a -> Stream f m x) -> Stream f m r
for (Stream step state) f = Stream step' (Left state)
  where
    {-# INLINE_LATE step' #-}
    step' (Left st) = do
        nxt <- step st
        case nxt of
            Yield (a :> rest) -> do
                return $ Skip (Right (f a, rest))
            Skip s -> return $ Skip (Left s)
            Return r -> return $ Return r
    step' (Right ((Stream ostep ost), st)) = do
        nxt <- ostep ost
        return $
            case nxt of
                Yield fs ->
                    Yield ((\os -> Right ((Stream ostep os), st)) `fmap` fs)
                Skip s -> Skip (Right ((Stream ostep s), st))
                Return _ -> Skip (Left st)

{-# INLINE_NORMAL scanl' #-}
scanl' :: Monad m => (b -> a -> b) -> b -> Stream (Of a) m r -> Stream (Of b) m r
scanl' fstep begin (Stream step state) = Stream step' (Right (state, begin))
  where
    {-# INLINE_LATE step' #-}
    step' (Left r) = return $ Return r
    step' (Right (st, acc)) = acc `seq` do
        nxt <- step st
        return $ case nxt of
            Yield (x :> rest) -> let !y = fstep acc x in Yield (acc :> Right (rest, y))
            Skip s -> Skip (Right (s, acc))
            Return r -> Yield (acc :> Left r)

{-# INLINE_NORMAL zipWithM #-}
zipWithM ::
       Monad m
    => (a -> b -> m c)
    -> Stream (Of a) m r
    -> Stream (Of b) m r
    -> Stream (Of c) m r
zipWithM f (Stream stepa ta) (Stream stepb tb) = Stream step' (ta, tb, Nothing)
  where
    {-# INLINE_LATE step' #-}
    step' (sa, sb, Nothing) = do
        nxt <- stepa sa
        return $
            case nxt of
                Yield (x :> sa') -> Skip (sa', sb, Just x)
                Skip sa' -> Skip (sa', sb, Nothing)
                Return r -> Return r
    step' (sa, sb, Just x) = do
        nxt <- stepb sb
        case nxt of
            Yield (y :> sb') -> do
                z <- f x y
                return $ Yield (z :> (sa, sb', Nothing))
            Skip sb' -> return $ Skip (sa, sb', Just x)
            Return r -> return $ Return r

{-# INLINE zipWith #-}
zipWith ::
       Monad m
    => (a -> b -> c)
    -> Stream (Of a) m r
    -> Stream (Of b) m r
    -> Stream (Of c) m r
zipWith f = zipWithM (\a b -> return (f a b))

{-# INLINE_NORMAL foldlM' #-}
foldlM' :: Monad m => (b -> a -> m b) -> b -> Stream (Of a) m r -> m (Of b r)
foldlM' fstep begin (Stream step state) = go SPEC state begin
  where
    go !_ st acc = acc `seq` do
       nxt <- step st
       case nxt of
           Yield (x :> rest) -> do
               acc' <- fstep acc x
               go SPEC rest acc'
           Skip s -> go SPEC s acc
           Return r -> return (acc :> r)

{-# INLINE foldl' #-}
foldl' :: Monad m => (b -> a -> b) -> b -> Stream (Of a) m r -> m (Of b r)
foldl' f = foldlM' (\b a -> return $ f b a)

{-# INLINE_NORMAL foldlM'_ #-}
foldlM'_ :: Monad m => (b -> a -> m b) -> b -> Stream (Of a) m r -> m b
foldlM'_ fstep begin (Stream step state) = go SPEC state begin
  where
    go !_ st acc = acc `seq` do
       nxt <- step st
       case nxt of
           Yield (x :> rest) -> do
               acc' <- fstep acc x
               go SPEC rest acc'
           Skip s -> go SPEC s acc
           Return _ -> return acc

{-# INLINE foldl'_ #-}
foldl'_ :: Monad m => (b -> a -> b) -> b -> Stream (Of a) m r -> m b
foldl'_ fstep = foldlM'_ (\b a -> return (fstep b a))

{-# INLINE_NORMAL foldrM #-}
foldrM :: Monad m => (a -> b -> m b) -> b -> Stream (Of a) m r -> m (Of b r)
foldrM f z (Stream step state) = go SPEC state
  where
    go !_ st = do
        nxt <- step st
        case nxt of
            Yield (a :> s) -> do
                (acc :> r) <- go SPEC s
                acc' <- f a acc
                return (acc' :> r)
            Skip s -> go SPEC s
            Return r -> return (z :> r)

{-# INLINE_NORMAL foldrM_ #-}
foldrM_ :: Monad m => (a -> b -> m b) -> b -> Stream (Of a) m r -> m b
foldrM_ f z (Stream step state) = go SPEC state
  where
    go !_ st = do
        nxt <- step st
        case nxt of
            Yield (a :> s) -> go SPEC s >>= f a
            Skip s -> go SPEC s
            Return _ -> return z

{-# INLINE last #-}
last :: Monad m => Stream (Of a) m r -> m (Of (Maybe a) r)
last = foldl' (\_ y -> Just y) Nothing

{-# INLINE last_ #-}
last_ :: Monad m => Stream (Of a) m r -> m (Maybe a)
last_ = foldl'_ (\_ y -> Just y) Nothing

{-# INLINE_NORMAL sum #-}
sum :: (Monad m, Num a) => Stream (Of a) m r -> m (Of a r)
sum (Stream step state) = go SPEC state
  where
    go !_ st = do
        nxt <- step st
        case nxt of
            Yield (x :> rest) ->
                (\(acc :> r) -> ((x + acc) :> r)) `fmap` (go SPEC rest)
            Skip s -> go SPEC s
            Return r -> return (0 :> r)

{-# INLINE sum_ #-}
sum_ :: (Monad m, Num a) => Stream (Of a) m r -> m a
sum_ = foldl'_ (+) 0{-SPEC state 0
  where
    go !_ st acc = acc `seq` do
        nxt <- step st
        case nxt of
            Yield (x :> rest) -> go SPEC rest (acc + x)
            Skip s -> go SPEC s acc
            Return _ -> return acc-}

{-# INLINE_NORMAL toList #-}
toList :: Monad m => Stream (Of a) m r -> m (Of [a] r)
toList (Stream step state) = go SPEC state
  where
    go !_ st = do
        nxt <- step st
        case nxt of
            Yield (x :> rest) ->
                (\(xs :> r) -> ((x : xs) :> r)) `fmap` (go SPEC rest)
            Skip s -> go SPEC s
            Return r -> return ([] :> r)

{-# INLINE_NORMAL toList_ #-}
toList_ :: Monad m => Stream (Of a) m r -> m [a]
toList_ (Stream step state) = go SPEC state
  where
    go !_ st = do
        nxt <- step st
        case nxt of
            Yield (x :> rest) -> (x :) `fmap` (go SPEC rest)
            Skip s -> go SPEC s
            Return _ -> return []

{-# INLINE_NORMAL yield #-}
yield :: Monad m => a -> Stream (Of a) m ()
yield x = Stream step True
  where
    {-# INLINE_LATE step #-}
    step True = return $ Yield (x :> False)
    step False = return $ Return ()

{-# INLINE stdinLn #-}
stdinLn :: MonadIO m => Stream (Of String) m ()
stdinLn = fromHandle stdin

{-# INLINE_NORMAL fromHandle #-}
fromHandle :: MonadIO m => Handle -> Stream (Of String) m ()
fromHandle h = Stream step ()
  where
    {-# INLINE_LATE step #-}
    step _ = do
        eof <- liftIO $ hIsEOF h
        if eof
            then return $ Return ()
            else do
                str <- liftIO $ hGetLine h
                return $ Yield (str :> ())

{-# INLINE_NORMAL toHandle #-}
toHandle :: MonadIO m => Handle -> Stream (Of String) m r -> m r
toHandle h (Stream step state) = go SPEC state
  where
    go !_ st = do
        nxt <- step st
        case nxt of
            Yield (str :> rest) -> do
                liftIO (hPutStrLn h str)
                go SPEC rest
            Skip s -> go SPEC s
            Return r -> return r

{-# INLINE_NORMAL readLn #-}
readLn :: (MonadIO m, Read a) => Stream (Of a) m ()
readLn = Stream step ()
  where
    {-# INLINE_LATE step #-}
    step _ = do
        eof <- liftIO isEOF
        if eof
            then return $ Return ()
            else do
                str <- liftIO getLine
                return $ case readMaybe str of
                    Nothing -> Skip ()
                    Just s -> Yield (s :> ())

{-# INLINE_NORMAL effects #-}
effects :: Monad m => Stream (Of a) m r -> m r
effects (Stream step state) = go SPEC state
  where
    go !_ st = do
        nxt <- step st
        case nxt of
            Yield (_ :> rst) -> go SPEC rst
            Skip s -> go SPEC s
            Return r -> return r

{-# INLINE_NORMAL printStream #-}
printStream :: (MonadIO m, Show a) =>Stream (Of a) m r -> m r
printStream (Stream step state) = go SPEC state
  where
    go !_ st = do
        nxt <- step st
        case nxt of
            Yield (x :> rst) -> do
                liftIO (print x)
                go SPEC rst
            Skip  s -> go SPEC s
            Return r -> return r

{-# INLINE_LATE toStreamK #-}
toStreamK :: (Functor f, Monad m) => Stream f m r -> K.Stream f m r
toStreamK (Stream step state) = go state
  where
    go st = K.Stream $ \yld sng stp -> do
        nxt <- step st
        case nxt of
            Yield fs -> yld (fmap go fs)
            Skip s -> K.unStream (go s) yld sng stp
            Return r -> stp r

{-# RULES "fromStreamK/toStreamK fusion"
    forall s. toStreamK (fromStreamK s) = s #-}
{-# RULES "toStreamK/fromStreamK fusion"
    forall s. fromStreamK (toStreamK s) = s #-}

{-# INLINE_LATE fromStreamK #-}
fromStreamK :: (Functor f, Monad m) => K.Stream f m r -> Stream f m r
fromStreamK = Stream step
  where
    step m1 =
        let stp r = return $ Return r
            single fr = return $ Yield (fmap K.nil fr)
            yieldk fs = return $ Yield fs
         in K.unStream m1 yieldk single stp
