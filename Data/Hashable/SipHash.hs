{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving #-}

module Data.Hashable.SipHash
    (
      LE64
    , fromWord64
    , fullBlock
    , lastBlock
    , finalize
    , hashByteString
    ) where

#include "MachDeps.h"

import Data.Bits
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.ByteString.Internal
import Foreign.Storable
import Numeric

newtype LE64 = LE64 { fromLE64 :: Word64 }
    deriving (Eq)

instance Show LE64 where
    show (LE64 !v) = let s = showHex v ""
                     in "0x" ++ replicate (16 - length s) '0' ++ s

fromWord64 :: Word64 -> LE64
#ifndef WORDS_BIGENDIAN
fromWord64 = LE64
#else
#error big endian support TBD
#endif

initState :: (Word64 -> Word64 -> Word64 -> Word64 -> r)
          -> Word64 -> Word64
          -> r
initState k k0 k1 = k v0 v1 v2 v3
    where !v0 = (k0 `xor` 0x736f6d6570736575)
          !v1 = (k1 `xor` 0x646f72616e646f6d)
          !v2 = (k0 `xor` 0x6c7967656e657261)
          !v3 = (k1 `xor` 0x7465646279746573)

sipRound :: (Word64 -> Word64 -> Word64 -> Word64 -> r)
         ->  Word64 -> Word64 -> Word64 -> Word64 -> r
sipRound k !v0 !v1 !v2 !v3 = k v0_c v1_d v2_c v3_d
  where v0_a  = v0 + v1
        v2_a  = v2 + v3
        v1_a  = v1 `rotateL` 13
        v3_a  = v3 `rotateL` 16
        v1_b  = v1_a `xor` v0_a
        v3_b  = v3_a `xor` v2_a
        v0_b  = v0_a `rotateL` 32
        v2_b  = v2_a + v1_b
        !v0_c = v0_b + v3_b
        v1_c  = v1_b `rotateL` 17
        v3_c  = v3_b `rotateL` 21
        !v1_d = v1_c `xor` v2_b
        !v3_d = v3_c `xor` v0_c
        !v2_c = v2_b `rotateL` 32

fullBlock :: Int -> LE64
          -> (Word64 -> Word64 -> Word64 -> Word64 -> r)
          ->  Word64 -> Word64 -> Word64 -> Word64 -> r
fullBlock c m k v0 v1 v2 v3 = runRounds c k' v0 v1 v2 (v3 `xor` fromLE64 m)
  where k' w0 = k $! (w0 `xor` fromLE64 m)
{-# INLINE fullBlock #-}

runRounds :: Int
          -> (Word64 -> Word64 -> Word64 -> Word64 -> r)
          ->  Word64 -> Word64 -> Word64 -> Word64 -> r
runRounds !c k = go 0
  where go i !v0 !v1 !v2 !v3
            | i < c     = sipRound (go (i+1)) v0 v1 v2 v3
            | otherwise = k v0 v1 v2 v3
{-# INLINE runRounds #-}

lastBlock :: Int -> Int -> LE64
          -> (Word64 -> Word64 -> Word64 -> Word64 -> r)
          ->  Word64 -> Word64 -> Word64 -> Word64 -> r
lastBlock !c !len !m k !v0 !v1 !v2 !v3 =
#ifndef WORDS_BIGENDIAN
    fullBlock c (LE64 m') k v0 v1 v2 v3
#else
#error big endian support TBD
#endif
  where m' = fromLE64 m .|. ((fromIntegral len .&. 0xff) `shiftL` 56)
{-# INLINE lastBlock #-}

finalize ::  Int
         -> (Word64 -> r)
         ->  Word64 -> Word64 -> Word64 -> Word64 -> r
finalize d k v0 v1 v2 v3 = runRounds d k' v0 v1 (v2 `xor` 0xff) v3
  where k' w0 w1 w2 w3 = k $! w0 `xor` w1 `xor` w2 `xor` w3
{-# INLINE finalize #-}

hashByteString :: Int -> Int -> Word64 -> Word64 -> ByteString -> Word64
hashByteString !c !d k0 k1 (PS fp off len) =
  inlinePerformIO . withForeignPtr fp $ \basePtr ->
    let ptr0 = basePtr `plusPtr` off
        scant = len .&. 7
        endBlocks = ptr0 `plusPtr` (len - scant)
        go !ptr !v0 !v1 !v2 !v3
            | ptr == endBlocks = readLast ptr 0 0
            | otherwise = do
                m <- peekLE64 ptr
                fullBlock c m (go (ptr `plusPtr` 8)) v0 v1 v2 v3
          where
            readLast p !s !m
                | p == end = lastBlock c len (LE64 m)
                             (finalize d return)
                             v0 v1 v2 v3
                | otherwise = do
                    b <- fromIntegral `fmap` peekByte p
                    readLast (p `plusPtr` 1) (s+8) (m .|. (b `unsafeShiftL` s))
              where end = ptr0 `plusPtr` len
    in initState (go ptr0) k0 k1

peekByte :: Ptr Word8 -> IO Word8
peekByte = peek

peekLE64 :: Ptr Word8 -> IO LE64
#if defined(x86_64_HOST_ARCH) || defined(i386_HOST_ARCH)
-- platforms on which unaligned loads are legal and usually fast
peekLE64 p = LE64 `fmap` peek (castPtr p)
#else
peekLE64 p = do
  let peek8 d = fromIntegral `fmap` peekByte (p `plusPtr` d)
  b0 <- peek8 0
  b1 <- peek8 1
  b2 <- peek8 2
  b3 <- peek8 3
  b4 <- peek8 4
  b5 <- peek8 5
  b6 <- peek8 6
  b7 <- peek8 7
  let !w = (b7 `shiftL` 56) .|. (b6 `shiftL` 48) .|. (b5 `shiftL` 40) .|.
           (b4 `shiftL` 32) .|. (b3 `shiftL` 24) .|. (b2 `shiftL` 16) .|.
           (b1 `shiftL` 8) .|. b0
  return (fromWord64 w)
#endif
