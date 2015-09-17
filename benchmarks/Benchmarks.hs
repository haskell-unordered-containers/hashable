{-# LANGUAGE BangPatterns, CPP, ForeignFunctionInterface, MagicHash,
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
import Foreign.C.Types (CInt(..), CLong(..), CSize(..))
import Foreign.Ptr
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Crypto.MAC.SipHash as HS
import qualified Data.ByteString.Char8 as B8

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
    let !ba5 = new 5;     !ba8 = new 8;     !ba11 = new 11; !ba40 = new 40
        !ba128 = new 128; !ba512 = new 512; !ba1Mb = new mb

        s5 = ['\0'..'\4'];   s8 = ['\0'..'\7'];     s11 = ['\0'..'\10']
        s40 = ['\0'..'\39']; s128 = ['\0'..'\127']; s512 = ['\0'..'\511']
        s1Mb = ['\0'..'\999999']

        !bs5 = B8.pack s5;   !bs8 = B8.pack s8;     !bs11 = B8.pack s11
        !bs40 = B8.pack s40; !bs128 = B8.pack s128; !bs512 = B8.pack s512
        !bs1Mb = B8.pack s1Mb

        blmeg = BL.take (fromIntegral mb) . BL.fromChunks . repeat
        bl5 = BL.fromChunks [bs5];     bl8 = BL.fromChunks [bs8]
        bl11 = BL.fromChunks [bs11];   bl40 = BL.fromChunks [bs40]
        bl128 = BL.fromChunks [bs128]; bl512 = BL.fromChunks [bs512]
        bl1Mb_40 = blmeg bs40;         bl1Mb_128 = blmeg bs128
        bl1Mb_64k = blmeg (B8.take 65536 bs1Mb)

        !t5 = T.pack s5;   !t8 = T.pack s8;     !t11 = T.pack s11
        !t40 = T.pack s40; !t128 = T.pack s128; !t512 = T.pack s512
        !t1Mb = T.pack s1Mb

        tlmeg = TL.take (fromIntegral mb) . TL.fromChunks . repeat
        tl5 = TL.fromStrict t5;     tl8 = TL.fromStrict t8
        tl11 = TL.fromStrict t11;   tl40 = TL.fromStrict t40
        tl128 = TL.fromStrict t128; tl512 = TL.fromChunks (replicate 4 t128)
        tl1Mb_40 = tlmeg t40;       tl1Mb_128 = tlmeg t128
        tl1Mb_64k = tlmeg (T.take 65536 t1Mb)

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
        fnvHash (PS fp off len) =
            inlinePerformIO . withForeignPtr fp $ \ptr ->
            return $! fnv_hash (ptr `plusPtr` off) (fromIntegral len) 2166136261
#ifdef HAVE_SSE2
        sse2SipHash (PS fp off len) =
            inlinePerformIO . withForeignPtr fp $ \ptr ->
            return $! sse2_siphash k0 k1 (ptr `plusPtr` off) (fromIntegral len)
#endif
#ifdef HAVE_SSE41
        sse41SipHash (PS fp off len) =
            inlinePerformIO . withForeignPtr fp $ \ptr ->
            return $! sse41_siphash k0 k1 (ptr `plusPtr` off) (fromIntegral len)
#endif

    withForeignPtr fp5 $ \ p5 ->
        withForeignPtr fp8 $ \ p8 ->
        withForeignPtr fp11 $ \ p11 ->
        withForeignPtr fp40 $ \ p40 ->
        withForeignPtr fp128 $ \ p128 ->
        withForeignPtr fp512 $ \ p512 ->
        withForeignPtr fp1Mb $ \ p1Mb ->
        defaultMain
        [ bgroup "hashPtr"
          [ bench "5" $ whnfIO $ hashPtr p5 5
          , bench "8" $ whnfIO $ hashPtr p8 8
          , bench "11" $ whnfIO $ hashPtr p11 11
          , bench "40" $ whnfIO $ hashPtr p40 40
          , bench "128" $ whnfIO $ hashPtr p128 128
          , bench "512" $ whnfIO $ hashPtr p512 512
          , bench "2^20" $ whnfIO $ hashPtr p1Mb mb
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
            [ bgroup "strict"
              [ bench "5" $ whnf hash bs5
              , bench "8" $ whnf hash bs8
              , bench "11" $ whnf hash bs11
              , bench "40" $ whnf hash bs40
              , bench "128" $ whnf hash bs128
              , bench "512" $ whnf hash bs512
              , bench "2^20" $ whnf hash bs1Mb
              ]
            , bgroup "lazy"
                [ bench "5" $ whnf hash bl5
                , bench "8" $ whnf hash bl8
                , bench "11" $ whnf hash bl11
                , bench "40" $ whnf hash bl40
                , bench "128" $ whnf hash bl128
                , bench "512" $ whnf hash bl512
                , bench "2^20_40" $ whnf hash bl1Mb_40
                , bench "2^20_128" $ whnf hash bl1Mb_128
                , bench "2^20_64k" $ whnf hash bl1Mb_64k
                ]
            ]
          , bgroup "String"
            [ bench "5" $ whnf hash s5
            , bench "8" $ whnf hash s8
            , bench "11" $ whnf hash s11
            , bench "40" $ whnf hash s40
            , bench "128" $ whnf hash s128
            , bench "512" $ whnf hash s512
            , bench "2^20" $ whnf hash s1Mb
            ]
          , bgroup "Text"
            [ bgroup "strict"
              [ bench "5" $ whnf hash t5
              , bench "8" $ whnf hash t8
              , bench "11" $ whnf hash t11
              , bench "40" $ whnf hash t40
              , bench "128" $ whnf hash t128
              , bench "512" $ whnf hash t512
              , bench "2^20" $ whnf hash t1Mb
              ]
            , bgroup "lazy"
              [ bench "5" $ whnf hash tl5
              , bench "8" $ whnf hash tl8
              , bench "11" $ whnf hash tl11
              , bench "40" $ whnf hash tl40
              , bench "128" $ whnf hash tl128
              , bench "512" $ whnf hash tl512
              , bench "2^20_40" $ whnf hash tl1Mb_40
              , bench "2^20_128" $ whnf hash tl1Mb_128
              , bench "2^20_64k" $ whnf hash tl1Mb_64k
              ]
            ]
          , bench "Int8" $ whnf hash (0xef :: Int8)
          , bench "Int16" $ whnf hash (0x7eef :: Int16)
          , bench "Int32" $ whnf hash (0x7eadbeef :: Int32)
          , bench "Int" $ whnf hash (0x7eadbeefdeadbeef :: Int)
          , bench "Int64" $ whnf hash (0x7eadbeefdeadbeef :: Int64)
          , bench "Double" $ whnf hash (0.3780675796601578 :: Double)
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
          [ bench "5" $ whnf cSipHash24 bs5
          , bench "8" $ whnf cSipHash24 bs8
          , bench "11" $ whnf cSipHash24 bs11
          , bench "40" $ whnf cSipHash24 bs40
          , bench "128" $ whnf cSipHash24 bs128
          , bench "512" $ whnf cSipHash24 bs512
          , bench "2^20" $ whnf cSipHash24 bs1Mb
          ]
#ifdef HAVE_SSE2
        , bgroup "sse2SipHash"
          [ bench "5" $ whnf sse2SipHash bs5
          , bench "8" $ whnf sse2SipHash bs8
          , bench "11" $ whnf sse2SipHash bs11
          , bench "40" $ whnf sse2SipHash bs40
          , bench "128" $ whnf sse2SipHash bs128
          , bench "512" $ whnf sse2SipHash bs512
          , bench "2^20" $ whnf sse2SipHash bs1Mb
          ]
#endif
#ifdef HAVE_SSE41
        , bgroup "sse41SipHash"
          [ bench "5" $ whnf sse41SipHash bs5
          , bench "8" $ whnf sse41SipHash bs8
          , bench "11" $ whnf sse41SipHash bs11
          , bench "40" $ whnf sse41SipHash bs40
          , bench "128" $ whnf sse41SipHash bs128
          , bench "512" $ whnf sse41SipHash bs512
          , bench "2^20" $ whnf sse41SipHash bs1Mb
          ]
#endif
        , bgroup "pkgSipHash"
          [ bench "5" $ whnf hsSipHash bs5
          , bench "8" $ whnf hsSipHash bs8
          , bench "11" $ whnf hsSipHash bs11
          , bench "40" $ whnf hsSipHash bs40
          , bench "128" $ whnf hsSipHash bs128
          , bench "512" $ whnf hsSipHash bs512
          , bench "2^20" $ whnf hsSipHash bs1Mb
          ]
        , bgroup "fnv"
          [ bench "5" $ whnf fnvHash bs5
          , bench "8" $ whnf fnvHash bs8
          , bench "11" $ whnf fnvHash bs11
          , bench "40" $ whnf fnvHash bs40
          , bench "128" $ whnf fnvHash bs128
          , bench "512" $ whnf fnvHash bs512
          , bench "2^20" $ whnf fnvHash bs1Mb
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
#ifdef HAVE_SSE2
foreign import ccall unsafe "hashable_siphash24_sse2" sse2_siphash
    :: Word64 -> Word64 -> Ptr Word8 -> CSize -> Word64
#endif
#ifdef HAVE_SSE41
foreign import ccall unsafe "hashable_siphash24_sse41" sse41_siphash
    :: Word64 -> Word64 -> Ptr Word8 -> CSize -> Word64
#endif

foreign import ccall unsafe "hashable_fnv_hash" fnv_hash
    :: Ptr Word8 -> CLong -> CLong -> CLong

foreign import ccall unsafe "hashable_wang_32" hash_wang_32
    :: Word32 -> Word32
foreign import ccall unsafe "hashable_wang_64" hash_wang_64
    :: Word64 -> Word64
foreign import ccall unsafe "hash_jenkins_32a" hash_jenkins_32a
    :: Word32 -> Word32
foreign import ccall unsafe "hash_jenkins_32b" hash_jenkins_32b
    :: Word32 -> Word32
