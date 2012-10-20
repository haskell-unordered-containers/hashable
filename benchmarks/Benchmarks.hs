{-# LANGUAGE BangPatterns, ForeignFunctionInterface, MagicHash,
    UnboxedTuples #-}

module Main (main) where

import Control.Monad.ST
import Criterion.Main
import Data.Hashable
import Data.Hashable.SipHash
import Data.Int
import Foreign.ForeignPtr
import GHC.Exts
import GHC.ST (ST(..))
import Data.Word
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Ptr
import Data.ByteString.Internal
import qualified Data.ByteString as B
import qualified Crypto.MAC.SipHash as HS

-- Benchmark English words (5 and 8), base64 encoded integers (11),
-- SHA1 hashes as hex (40), and large blobs (1 Mb).
main :: IO ()
main = do
    -- We do not actually care about the contents of these pointers.
    fp5 <- mallocForeignPtrBytes 5
    fp8 <- mallocForeignPtrBytes 8
    fp11 <- mallocForeignPtrBytes 11
    fp40 <- mallocForeignPtrBytes 40
    fp128 <- mallocForeignPtrBytes 128
    fp512 <- mallocForeignPtrBytes 512
    let !mb = 2^(20 :: Int)  -- 1 Mb
    fp1Mb <- mallocForeignPtrBytes mb

    -- We don't care about the contents of these either.
    let !ba5 = new 5
        !ba8 = new 8
        !ba11 = new 11
        !ba40 = new 40
        !ba128 = new 128
        !ba512 = new 512
        !ba1Mb = new mb

    let !bs5 = B.pack [0..4]
        !bs8 = B.pack [0..7]
        !bs11 = B.pack [0..10]
        !bs40 = B.pack [0..39]
        !bs128 = B.pack [0..127]
        !bs512 = B.pack . map fromIntegral $ [0..511::Int]
        !bs1Mb = B.pack . map fromIntegral $ [0..999999::Int]

    let k0 = 0x4a7330fae70f52e8
        k1 = 0x919ea5953a9a1ec9
        sipHash = hashByteString 2 4 k0 k1
        hsSipHash = HS.hash (HS.SipKey k0 k1)
        cSipHash (PS fp off len) =
            inlinePerformIO . withForeignPtr fp $ \ptr ->
            return $! c_siphash 2 4 k0 k1 (ptr `plusPtr` off) (fromIntegral len)
        cSipHash24 (PS fp off len) =
            inlinePerformIO . withForeignPtr fp $ \ptr ->
            return $! c_siphash24 k0 k1 (ptr `plusPtr` off) (fromIntegral len)
        sse41SipHash (PS fp off len) =
            inlinePerformIO . withForeignPtr fp $ \ptr ->
            return $! sse41_siphash k0 k1 (ptr `plusPtr` off) (fromIntegral len)
        cSipHash8 v = c_siphash24_u8 k0 k1 (fromIntegral v)
        cSipHash16 v = c_siphash24_u16 k0 k1 (fromIntegral v)
        cSipHash32 v = c_siphash24_u32 k0 k1 (fromIntegral v)
        cSipHash64 v = c_siphash24_u64 k0 k1 (fromIntegral v)

    withForeignPtr fp5 $ \ p5 ->
        withForeignPtr fp8 $ \ p8 ->
        withForeignPtr fp11 $ \ p11 ->
        withForeignPtr fp40 $ \ p40 ->
        withForeignPtr fp128 $ \ p128 ->
        withForeignPtr fp512 $ \ p512 ->
        withForeignPtr fp1Mb $ \ p1Mb ->
        defaultMain
        [ bgroup "hashPtr"
          [ bench "5" $ hashPtr p5 5
          , bench "8" $ hashPtr p8 8
          , bench "11" $ hashPtr p11 11
          , bench "40" $ hashPtr p40 40
          , bench "128" $ hashPtr p128 128
          , bench "512" $ hashPtr p512 512
          , bench "2^20" $ hashPtr p1Mb mb
          ]
        , bgroup "hashByteArray"
          [ bench "5" $ whnf (hashByteArray ba5 0) 5
          , bench "8" $ whnf (hashByteArray ba8 0) 8
          , bench "11" $ whnf (hashByteArray ba11 0) 11
          , bench "40" $ whnf (hashByteArray ba40 0) 40
          , bench "128" $ whnf (hashByteArray ba128 0) 128
          , bench "512" $ whnf (hashByteArray ba512 0) 512
          , bench "2^20" $ whnf (hashByteArray ba1Mb 0) mb
          ]
        , bgroup "hash"
          [ bgroup "ByteString"
            [ bench "5" $ whnf hash bs5
            , bench "8" $ whnf hash bs8
            , bench "11" $ whnf hash bs11
            , bench "40" $ whnf hash bs40
            , bench "128" $ whnf hash bs128
            , bench "512" $ whnf hash bs512
            , bench "2^20" $ whnf hash bs1Mb
            ]
          , bench "Int8" $ whnf hash (0xef :: Int8)
          , bench "Int16" $ whnf hash (0x7eef :: Int16)
          , bench "Int32" $ whnf hash (0x7eadbeef :: Int32)
          , bench "Int" $ whnf hash (0x7eadbeefdeadbeef :: Int)
          , bench "Int64" $ whnf hash (0x7eadbeefdeadbeef :: Int64)
          ]
        , bgroup "sipHash"
          [ bench "5" $ whnf sipHash bs5
          , bench "8" $ whnf sipHash bs8
          , bench "11" $ whnf sipHash bs11
          , bench "40" $ whnf sipHash bs40
          , bench "128" $ whnf sipHash bs128
          , bench "512" $ whnf sipHash bs512
          , bench "2^20" $ whnf sipHash bs1Mb
          ]
        , bgroup "cSipHash"
          [ bench "5" $ whnf cSipHash bs5
          , bench "8" $ whnf cSipHash bs8
          , bench "11" $ whnf cSipHash bs11
          , bench "40" $ whnf cSipHash bs40
          , bench "128" $ whnf cSipHash bs128
          , bench "512" $ whnf cSipHash bs512
          , bench "2^20" $ whnf cSipHash bs1Mb
          ]
        , bgroup "cSipHash24"
          [ bgroup "ByteString"
            [ bench "5" $ whnf cSipHash24 bs5
            , bench "8" $ whnf cSipHash24 bs8
            , bench "11" $ whnf cSipHash24 bs11
            , bench "40" $ whnf cSipHash24 bs40
            , bench "128" $ whnf cSipHash24 bs128
            , bench "512" $ whnf cSipHash24 bs512
            , bench "2^20" $ whnf cSipHash24 bs1Mb
            ]
          , bench "Int8" $ whnf cSipHash8 (0x5a :: Int8)
          , bench "Int16" $ whnf cSipHash16 (0x5a5a :: Int16)
          , bench "Int32" $ whnf cSipHash32 (0x5a5a5a5a :: Int32)
          , bench "Int64" $ whnf cSipHash64 (0x5a5a5a5a5a5a5a5a :: Int64)
          ]
        , bgroup "sse41SipHash"
          [ bench "5" $ whnf sse41SipHash bs5
          , bench "8" $ whnf sse41SipHash bs8
          , bench "11" $ whnf sse41SipHash bs11
          , bench "40" $ whnf sse41SipHash bs40
          , bench "128" $ whnf sse41SipHash bs128
          , bench "512" $ whnf sse41SipHash bs512
          , bench "2^20" $ whnf sse41SipHash bs1Mb
          ]
        , bgroup "pkgSipHash"
          [ bench "5" $ whnf hsSipHash bs5
          , bench "8" $ whnf hsSipHash bs8
          , bench "11" $ whnf hsSipHash bs11
          , bench "40" $ whnf hsSipHash bs40
          , bench "128" $ whnf hsSipHash bs128
          , bench "512" $ whnf hsSipHash bs512
          , bench "2^20" $ whnf hsSipHash bs1Mb
          ]
        , bgroup "Int"
          [ bench "id32"   $ whnf id           (0x7eadbeef :: Int32)
          , bench "id64"   $ whnf id           (0x7eadbeefdeadbeef :: Int64)
          , bench "wang32" $ whnf hash_wang_32 0xdeadbeef
          , bench "wang64" $ whnf hash_wang_64 0xdeadbeefdeadbeef
          , bench "jenkins32a" $ whnf hash_jenkins_32a 0xdeadbeef
          , bench "jenkins32b" $ whnf hash_jenkins_32b 0xdeadbeef
          ]
        ]

data ByteArray = BA { unBA :: !ByteArray# }

new :: Int -> ByteArray#
new (I# n#) = unBA (runST $ ST $ \s1 ->
    case newByteArray# n# s1 of
        (# s2, ary #) -> case unsafeFreezeByteArray# ary s2 of
            (# s3, ba #) -> (# s3, BA ba #))

foreign import ccall unsafe "hashable_siphash" c_siphash
    :: CInt -> CInt -> Word64 -> Word64 -> Ptr Word8 -> CSize -> Word64
foreign import ccall unsafe "hashable_siphash24" c_siphash24
    :: Word64 -> Word64 -> Ptr Word8 -> CSize -> Word64
foreign import ccall unsafe "hashable_siphash24_u64" c_siphash24_u64
    :: Word64 -> Word64 -> Word64 -> Word64
foreign import ccall unsafe "hashable_siphash24_u32" c_siphash24_u32
    :: Word64 -> Word64 -> Word32 -> Word64
foreign import ccall unsafe "hashable_siphash24_u16" c_siphash24_u16
    :: Word64 -> Word64 -> Word16 -> Word64
foreign import ccall unsafe "hashable_siphash24_u8" c_siphash24_u8
    :: Word64 -> Word64 -> Word8 -> Word64
foreign import ccall unsafe "hashable_siphash24_sse41" sse41_siphash
    :: Word64 -> Word64 -> Ptr Word8 -> CSize -> Word64

foreign import ccall unsafe "hash_wang_32" hash_wang_32
    :: Word32 -> Word32
foreign import ccall unsafe "hash_wang_64" hash_wang_64
    :: Word64 -> Word64
foreign import ccall unsafe "hash_jenkins_32a" hash_jenkins_32a
    :: Word32 -> Word32
foreign import ccall unsafe "hash_jenkins_32b" hash_jenkins_32b
    :: Word32 -> Word32
