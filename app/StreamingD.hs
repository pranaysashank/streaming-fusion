{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module StreamingD
    ( Stream
    , unfold
    , maps
    , run
    , concats
    , splitsAt
    , drop
    , each
    , filterM
    , filter
    , mapM
    , map
    , mapM_
    , sum
    , sum_
    , toList
    , toList_
    , effects
    , printStream
    ) where

import Control.Monad.IO.Class
import Prelude hiding (drop, filter, map, mapM, mapM_, pred, sum)

data Of a b = !a :> b

instance Functor (Of a) where
    fmap f (a :> x) = a :> (f x)

data Streaming f m r = Stepping !(f (Streaming f m r))
                     | Effecting (m (Streaming f m r))
                     | Returning r

data Stream f m r = forall s. Stream (s -> m (Step f s r)) s

data Step f s r = Yield !(f s)
                | Skip s
                | Return r

instance Monad m => Functor (Stream f m) where
  -- fmap :: (a -> b) -> Stream f m a -> Stream f m b
  fmap f (Stream step state) = Stream step' state
    where
      step' st = do
          nxt <- step st
          return $ case nxt of
              Yield fs -> Yield fs
              Skip   s -> Skip s
              Return r -> Return (f r)

instance (Functor f, Monad m) => Applicative (Stream f m) where
    pure r = Stream (\_ -> return $ Return r) ()

--  (<*>) :: Stream f m (a -> b) -> Stream f m a -> Stream f m b
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
    return = pure

--  (>>=) :: Stream f m a -> (a -> Stream f m b) -> Stream f m b
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

inspect :: (Functor f, Monad m) => Stream f m r -> m (Either r (f (Stream f m r)))
inspect (Stream step state) = go state
  where
    go st = do
        nxt <- step st
        case nxt of
            Yield fs -> return (Right (fmap (Stream step) fs))
            Skip s -> go s
            Return r -> return (Left r)

unfold :: Monad m => (s -> m (Either r (f s))) -> s -> Stream f m r
unfold gen state = (Stream step state)
  where
    step st = do
        nxt <- gen st
        case nxt of
            Left r -> return $ Return r
            Right fs -> return $ Yield (fs)

maps ::
       Monad m
    => (forall x. f x -> g x)
    -> Stream f m r
    -> Stream g m r
maps natTrans (Stream step state) = Stream step' state
  where
    step' st = do
        nxt <- step st
        return $
            case nxt of
                Yield fs -> Yield (natTrans fs)
                Skip s -> Skip s
                Return r -> Return r

run :: Monad m => Stream m m r -> m r
run (Stream step state) = go state
  where
    go st = do
        nxt <- step st
        case nxt of
            Yield fs -> fs >>= go
            Skip s -> go s
            Return r -> return r

concats :: (Monad m, Functor f) => Stream (Stream f m) m r -> Stream f m r
concats (Stream step state) = (Stream step' (Left state))
  where
    step' (Left st) = do
        nxt <- step st
        case nxt of
            Yield fs -> do
                x <- inspect fs
                return $
                    case x of
                        Left r -> Skip (Left r)
                        Right fsp -> Yield (Right `fmap` fsp)
            Skip s -> return $ Skip (Left s)
            Return r -> return $ Return r
    step' (Right (Stream istep ist)) = do
        nxt <- istep ist
        return $
            case nxt of
                Yield fs -> Yield (fmap (\s -> Right (Stream istep s)) fs)
                Skip s -> Skip (Right (Stream istep s))
                Return r -> Skip (Left r)


splitsAt ::
       (Monad m, Functor f) => Int -> Stream f m r -> Stream f m (Stream f m r)
splitsAt n str
    | n <= 0 = (Stream (\_ -> return $ Return str) ())
splitsAt n (Stream step state) = Stream step' (Left (state, n-1))
  where
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

drop :: Monad m => Int -> Stream (Of a) m r -> Stream (Of a) m r
drop n str
    | n <= 0 = str
drop n (Stream step state) = (Stream step' (state, 0))
  where
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

each :: Monad m => [a] -> Stream (Of a) m ()
each = Stream step
  where
    step (x:xs) = return $ Yield (x :> xs)
    step _ = return $ Return ()

filterM :: Monad m => (a -> m Bool) -> Stream (Of a) m r -> Stream (Of a) m r
filterM pred (Stream step state) = Stream step' state
  where
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

filter :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m r
filter f = filterM (return . f)

mapM :: Monad m => (a -> m b) -> Stream (Of a) m r -> Stream (Of b) m r
mapM f (Stream step state) = Stream step' state
  where
    step' st = do
        nxt <- step st
        case nxt of
            Yield (x :> rest) -> f x >>= \y -> return $ Yield (y :> rest)
            Skip s -> return $ Skip s
            Return r -> return $ Return r

map :: Monad m => (a -> b) -> Stream (Of a) m r -> Stream (Of b) m r
map f = mapM (return . f)

mapM_ :: Monad m => (a -> m b) -> Stream (Of a) m r -> m r
mapM_ f (Stream step state) = go state
  where
    go st = do
        nxt <- step st
        case nxt of
            Yield (x :> rest) -> f x *> go rest
            Skip s -> go s
            Return r -> return r

sum :: (Monad m, Num a) => Stream (Of a) m r -> m (Of a r)
sum (Stream step state) = go state
  where
    go st = do
        nxt <- step st
        case nxt of
            Yield (x :> rest) ->
                (\(acc :> r) -> ((x + acc) :> r)) `fmap` (go rest)
            Skip s -> go s
            Return r -> return (0 :> r)

sum_ :: (Monad m, Num a) => Stream (Of a) m r -> m a
sum_ (Stream step state) = go state
  where
    go st = do
        nxt <- step st
        case nxt of
            Yield (x :> rest) -> (x +) `fmap` (go rest)
            Skip s -> go s
            Return _ -> return 0

toList :: Monad m => Stream (Of a) m r -> m (Of [a] r)
toList (Stream step state) = go state
  where
    go st = do
        nxt <- step st
        case nxt of
            Yield (x :> rest) ->
                (\(xs :> r) -> ((x : xs) :> r)) `fmap` (go rest)
            Skip s -> go s
            Return r -> return ([] :> r)

toList_ :: Monad m => Stream (Of a) m r -> m [a]
toList_ (Stream step state) = go state
  where
    go st = do
        nxt <- step st
        case nxt of
            Yield (x :> rest) -> (x :) `fmap` (go rest)
            Skip s -> go s
            Return _ -> return []

effects :: Monad m => Stream (Of a) m r -> m r
effects (Stream step state) = go state
  where
    go st = do
        nxt <- step st
        case nxt of
            Yield (_ :> rst) -> go rst
            Skip s -> go s
            Return r -> return r

printStream :: (MonadIO m, Show a) =>Stream (Of a) m r -> m r
printStream (Stream step state) = go state
  where
    go st = do
        nxt <- step st
        case nxt of
            Yield (x :> rst) -> do
                liftIO (print x)
                go rst
            Skip  s -> go s
            Return r -> return r
