{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, MagicHash, Rank2Types,
    UnboxedTuples #-}

-- | Tests for the 'Data.Hashable' module.  We test functions by
-- comparing the C and Haskell implementations.

module Main (main) where

import Data.Hashable (hash, hashByteArray, hashPtr)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Foreign (unsafePerformIO)
import Foreign.Marshal.Array (withArray)
import GHC.Base (ByteArray#, Int(..), newByteArray#, unsafeCoerce#,
                 writeWord8Array#)
import GHC.ST (ST(..), runST)
import GHC.Word (Word8(..))
import System.Random
import Foreign.Storable
import Test.QuickCheck
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

------------------------------------------------------------------------
-- * Properties

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR  (a,b) g = case randomR (fromIntegral a :: Integer,
                                         fromIntegral b :: Integer) g of
                            (x,g') -> (fromIntegral x, g')

instance Arbitrary T.Text where
    arbitrary = T.pack `fmap` arbitrary

instance Arbitrary L.Text where
    arbitrary = L.pack `fmap` arbitrary

-- | Validate the implementation by comparing the C and Haskell
-- versions.
pHash :: [Word8] -> Bool
pHash xs = unsafePerformIO $ withArray xs $ \ p ->
    (hashByteArray (fromList xs) 0 len ==) `fmap` hashPtr p len
  where len = length xs

-- | Content equality implies hash equality.
pText :: T.Text -> T.Text -> Bool
pText a b = (a == b) == (hash a == hash b)

-- | Content equality implies hash equality.
pTextLazy :: L.Text -> L.Text -> Bool
pTextLazy a b = (a == b) == (hash a == hash b)

-- | Content equality implies hash equality.
pLazyRechunked :: T.Text -> [Int] -> Bool
pLazyRechunked t cs = hash (L.fromStrict t) == hash (rechunk t cs)

rechunk :: T.Text -> [Int] -> L.Text
rechunk t cs = L.fromChunks (go cs t)
  where
    go [] t = [t]
    go (c:cs) t | T.null t  = []
                | otherwise = let (a,b) = T.splitAt (c `mod` maxChunk) t
                              in a : go cs b
    maxChunk = 16

-- This wrapper is required by 'runST'.
data ByteArray = BA { unBA :: ByteArray# }

-- | Create a 'ByteArray#' from a list of 'Word8' values.
fromList :: [Word8] -> ByteArray#
fromList xs0 = unBA (runST $ ST $ \ s1# ->
    case newByteArray# len# s1# of
        (# s2#, marr# #) -> case go s2# 0 marr# xs0 of
            s3# -> (# s3#, BA (unsafeCoerce# marr#) #))
  where
    !(I# len#) = length xs0
    go s# _         _     []           = s#
    go s# i@(I# i#) marr# ((W8# x):xs) =
        case writeWord8Array# marr# i# x s# of
            s2# -> go s2# (i + 1) marr# xs

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = defaultMain tests

tests = [
    testProperty "bernstein" pHash
  , testGroup "text" [
      testProperty "text/strict" pText
    , testProperty "text/lazy" pTextLazy
    , testProperty "text/rechunked" pLazyRechunked
  ]
  ]
