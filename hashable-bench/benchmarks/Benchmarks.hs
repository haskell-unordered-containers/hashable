{-# LANGUAGE BangPatterns, CPP, ForeignFunctionInterface, MagicHash,
    UnboxedTuples, DeriveGeneric #-}

module Main (main) where

import Control.Monad.ST
import Criterion.Main
import Data.Hashable
import Data.Int
import Foreign.ForeignPtr
import GHC.Exts
import GHC.ST (ST(..))
import Data.ByteString.Internal
import GHC.Generics (Generic)
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

    let exP = P 22.0203 234.19 'x' 6424
        exS = S3
        exPS = PS3 'z' 7715

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

        hsSipHash :: ByteString -> HS.SipHash
        hsSipHash = HS.hash (HS.SipKey k0 k1)

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
          ]
        , bgroup "Generic"
          [ bench "product" $ whnf hash exP
          , bench "sum" $ whnf hash exS
          , bench "product and sum" $ whnf hash exPS
          ]
        ]

data ByteArray = BA { unBA :: !ByteArray# }

new :: Int -> ByteArray#
new (I# n#) = unBA (runST $ ST $ \s1 ->
    case newByteArray# n# s1 of
        (# s2, ary #) -> case unsafeFreezeByteArray# ary s2 of
            (# s3, ba #) -> (# s3, BA ba #))

data PS
  = PS1 Int Char Bool
  | PS2 String ()
  | PS3 Char Int
  deriving (Eq, Generic)

data P = P Double Float Char Int
  deriving (Eq, Generic)

data S = S1 | S2 | S3 | S4 | S5
  deriving (Eq, Generic)

instance Hashable PS
instance Hashable P
instance Hashable S
