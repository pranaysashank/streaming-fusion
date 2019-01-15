{-#LANGUAGE BangPatterns #-}
module Main (main) where

import qualified Streaming.Prelude as Str

import qualified Streamly.Prelude as S

import qualified StreamingD as StrD

import           Control.Exception
import           Criterion.Main
--
import           Data.Function ((&))

import           Test.Hspec
import           Test.Hspec.Expectations

big :: Int
big = 24012000

main :: IO ()
main = do
    defaultMain
      [ bgroup "sum"     [ bench "streaming"      $ nfIO streaming_sum
                         , bench "streamly"       $ nfIO streamly_sum
                         , bench "streamingD"     $ nfIO streamingD_sum
                         ]
      , bgroup "toList"  [ bench "streaming"      $ nfIO streaming_basic
                         , bench "streamly"        $ nfIO streamly_basic
                         , bench "streamingD"     $ nfIO streamingD_basic
                         ]
      ]

{-# INLINE streamly_basic #-}
streamly_basic :: IO Int
streamly_basic = do
    xs <- S.toList $
      S.fromList [1..10000]
      & S.filter even
      & S.map (+1)
      & S.drop 1000
      & S.map (+1)
      & S.filter (\x -> x `mod` 2 == 0)
    assert (Prelude.length xs == 4000) $
        return (Prelude.length (xs :: [Int]))

{-# INLINE streamly_sum #-}
streamly_sum :: IO Int
streamly_sum = do
    n <- S.sum $
      S.fromList [1..10000]
      & S.filter even
      & S.map (+1)
      & S.drop 1000
      & S.map (+1)
      & S.filter (\x -> x `mod` 2 == 0)
    assert (n == big) $
        return n

{-# INLINE streaming_basic #-}
streaming_basic :: IO Int
streaming_basic = do
    xs <- Str.toList_ $
      Str.each [1..10000]
      & Str.filter even
      & Str.map (+1)
      & Str.drop 1000
      & Str.map (+1)
      & Str.filter (\x -> x `mod` 2 == 0)
    assert (Prelude.length xs == 4000) $
        return (Prelude.length (xs :: [Int]))

{-# INLINE streaming_sum #-}
streaming_sum :: IO Int
streaming_sum = do
    n <- Str.sum_ $
      Str.each [1..10000]
      & Str.filter even
      & Str.map (+1)
      & Str.drop 1000
      & Str.map (+1)
      & Str.filter (\x -> x `mod` 2 == 0)
    assert (n == big) $
        return n

{-# INLINE streamingD_basic #-}
streamingD_basic :: IO Int
streamingD_basic = do
    xs <- StrD.toList_ $
      StrD.each [1..10000]
      & StrD.filter even
      & StrD.map (+1)
      & StrD.drop 1000
      & StrD.map (+1)
      & StrD.filter (\x -> x `mod` 2 == 0)
    assert (Prelude.length xs == 4000) $
        return (Prelude.length (xs :: [Int]))

{-# INLINE streamingD_sum #-}
streamingD_sum :: IO Int
streamingD_sum = do
    n <- StrD.sum_ $
      StrD.each [1..10000]
      & StrD.filter even
      & StrD.map (+1)
      & StrD.drop 1000
      & StrD.map (+1)
      & StrD.filter (\x -> x `mod` 2 == 0)
    assert (n == big) $
        return n
