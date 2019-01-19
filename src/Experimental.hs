-- This turned out to be pretty uninteresting because of the restriction
-- of the functor layer in a Monad (Stream f m).

module Experimental where

import Data.Bifunctor
import StreamingD (Stream(..), Step(..), maps)

newtype SerialT f m r a = SerialT
    { getSerialT :: Stream (f a) m r
    }

instance (Bifunctor f, Monad m) => Functor (SerialT f m r) where
    --fmap :: (a -> b) -> SerialT f m r a -> SerialT f m r b
    fmap f (SerialT stream) = SerialT $ maps (first f) stream

instance (Biapplicative f, Monad m, Monoid r) => Applicative (SerialT f m r) where
    -- pure :: a -> SerialT f m r a
    -- pure :: a -> Stream (f a) m r
    pure a = SerialT (Stream step False)
      where
        step False = return $ Yield (bipure a True)
        step True = return $ Return mempty

    -- (<*>) :: SerialT f m r (a -> b) -> SerialT f m r a -> SerialT f m r b
    -- (<*>) :: Stream (f (a -> b)) m r -> Stream (f a) m r -> Stream (f b) m r
    (SerialT (Stream step state)) <*> (SerialT (Stream istep istate)) = SerialT $ Stream step' (Left (state, istate))
      where
        step' (Left (st, ist)) = do
            nxt <- step st
            return $ case nxt of
                Yield fs -> Skip (Right (fs, ist))
                Skip   s -> Skip (Left (s, ist))
                Return r -> Return r

        step' (Right (ffs, ist)) = do
             nxt <- istep ist
             return $ case nxt of
                 Yield fas -> Yield ((second (\x y -> Left (x, y)) ffs) <<*>> fas)
                 Skip is -> Skip (Right (ffs, is))
                 Return r -> Return r

-- join :: Stream (f (Stream (f a) m r)) m r -> Stream (f a) m r
{-join :: SerialT f m r (SerialT f m r a) -> SerialT f m r a
join (Stream step state) = Stream step' (X state)
  where
    step' (X st) = do
        nxt <- step st
        return $ case nxt of
            Yield fas -> Skip (Y fas)-}

instance (Biapplicative f, Monad m, Monoid r) => Monad (SerialT f m r) where
    return = pure

    -- (>>=) :: SerialT f m r a -> (a -> SerialT f m r b) -> SerialT f m r b
    -- (>>=) :: Stream (f a) m r -> (a -> Stream (f b) m r) -> Stream (f b) m r
    {-(SerialT (Stream step state)) >>= f = SerialT (Stream step' (Left (state, mempty)))
      where
        {-# INLINE_LATE step' #-}
        step' (Left (st, acc)) = do
            nxt <- step st
            return $
                case nxt of
                    Yield fas -> Skip (Right $ first (\a -> getSerialT (f a)) fas))
                    Skip s -> Skip (Left (s, acc))
                    Return r -> Return (acc `mappend` r)
        step' (Right fbs) = do-}

-- Just for testing
class Bifunctor p => Biapplicative p where
    bipure :: a -> b -> p a b
    (<<*>>) :: p (a -> b) (c -> d) -> p a c -> p b d
