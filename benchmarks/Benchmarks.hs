{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}

module Main (main) where

import Control.Monad.ST
import Criterion.Main
import Data.Hashable
import Foreign.ForeignPtr
import GHC.Exts
import GHC.ST (ST(..))

-- Benchmark English words (5 and 8), base64 encoded integers (11),
-- SHA1 hashes as hex (40), and large blobs (1 Mb).
main :: IO ()
main = do
    -- We do not actually care about the contents of these pointers.
    fp5 <- mallocForeignPtrBytes 5
    fp8 <- mallocForeignPtrBytes 8
    fp11 <- mallocForeignPtrBytes 11
    fp40 <- mallocForeignPtrBytes 40
    let !mb = 2^(20 :: Int)  -- 1 Mb
    fp1Mb <- mallocForeignPtrBytes mb
    
    -- We don't care about the contents of these either.
    let !ba5 = new 5
        !ba8 = new 8
        !ba11 = new 11
        !ba40 = new 40
        !ba1Mb = new mb
    
    withForeignPtr fp5 $ \ p5 ->
        withForeignPtr fp8 $ \ p8 ->
        withForeignPtr fp11 $ \ p11 ->
        withForeignPtr fp40 $ \ p40 ->
        withForeignPtr fp1Mb $ \ p1Mb ->
        defaultMain
        [ bgroup "hashPtr"
          [ bench "5" $ hashPtr p5 5
          , bench "8" $ hashPtr p8 8
          , bench "11" $ hashPtr p11 11
          , bench "40" $ hashPtr p40 40
          , bench "2^20" $ hashPtr p1Mb mb
          ]
        , bgroup "hashByteArray"
          [ bench "5" $ whnf (hashByteArray ba5 0) 5
          , bench "8" $ whnf (hashByteArray ba8 0) 8
          , bench "11" $ whnf (hashByteArray ba11 0) 11
          , bench "40" $ whnf (hashByteArray ba40 0) 40
          , bench "2^20" $ whnf (hashByteArray ba1Mb 0) mb
          ]
        ]

data ByteArray = BA { unBA :: !ByteArray# }

new :: Int -> ByteArray#
new (I# n#) = unBA (runST $ ST $ \s1 ->
    case newByteArray# n# s1 of
        (# s2, ary #) -> case unsafeFreezeByteArray# ary s2 of
            (# s3, ba #) -> (# s3, BA ba #))
