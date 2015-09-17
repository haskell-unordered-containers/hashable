{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Data.Hashable.SipHash
    (
      LE64
    , Sip
    , fromWord64
    , fullBlock
    , lastBlock
    , finalize
    , hashByteString
    ) where

#include "MachDeps.h"

import Data.Bits ((.|.), (.&.), rotateL, shiftL, xor)
#if MIN_VERSION_base(4,5,0)
import Data.Bits (unsafeShiftL)
#endif
import Data.Word (Word8, Word64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Data.ByteString.Internal (ByteString(PS), inlinePerformIO)
import Foreign.Storable (peek)
import Numeric (showHex)

newtype LE64 = LE64 { fromLE64 :: Word64 }
    deriving (Eq)

instance Show LE64 where
    show (LE64 !v) = let s = showHex v ""
                     in "0x" ++ replicate (16 - length s) '0' ++ s

data Sip = Sip {
      v0 :: {-# UNPACK #-} !Word64, v1 :: {-# UNPACK #-} !Word64
    , v2 :: {-# UNPACK #-} !Word64, v3 :: {-# UNPACK #-} !Word64
    }

fromWord64 :: Word64 -> LE64
#ifndef WORDS_BIGENDIAN
fromWord64 = LE64
#else
#error big endian support TBD
#endif

initState :: (Sip -> r) -> Word64 -> Word64 -> r
initState k k0 k1 = k (Sip s0 s1 s2 s3)
    where !s0 = (k0 `xor` 0x736f6d6570736575)
          !s1 = (k1 `xor` 0x646f72616e646f6d)
          !s2 = (k0 `xor` 0x6c7967656e657261)
          !s3 = (k1 `xor` 0x7465646279746573)

sipRound :: (Sip -> r) -> Sip -> r
sipRound k Sip{..} = k (Sip v0_c v1_d v2_c v3_d)
  where v0_a = v0 + v1
        v2_a = v2 + v3
        v1_a = v1 `rotateL` 13
        v3_a = v3 `rotateL` 16
        v1_b = v1_a `xor` v0_a
        v3_b = v3_a `xor` v2_a
        v0_b = v0_a `rotateL` 32
        v2_b = v2_a + v1_b
        v0_c = v0_b + v3_b
        v1_c = v1_b `rotateL` 17
        v3_c = v3_b `rotateL` 21
        v1_d = v1_c `xor` v2_b
        v3_d = v3_c `xor` v0_c
        v2_c = v2_b `rotateL` 32

fullBlock :: Int -> LE64 -> (Sip -> r) -> Sip -> r
fullBlock c m k st@Sip{..}
    | c == 2    = sipRound (sipRound k') st'
    | otherwise = runRounds c k' st'
  where k' st1@Sip{..} = k st1{ v0 = v0 `xor` fromLE64 m }
        st'           = st{ v3 = v3 `xor` fromLE64 m }
{-# INLINE fullBlock #-}

runRounds :: Int -> (Sip -> r) -> Sip -> r
runRounds c k = go 0
  where go i st
            | i < c     = sipRound (go (i+1)) st
            | otherwise = k st
{-# INLINE runRounds #-}

lastBlock :: Int -> Int -> LE64 -> (Sip -> r) -> Sip -> r
lastBlock !c !len !m k st =
#ifndef WORDS_BIGENDIAN
    fullBlock c (LE64 m') k st
#else
#error big endian support TBD
#endif
  where m' = fromLE64 m .|. ((fromIntegral len .&. 0xff) `shiftL` 56)
{-# INLINE lastBlock #-}

finalize :: Int -> (Word64 -> r) -> Sip -> r
finalize d k st@Sip{..}
    | d == 4    = sipRound (sipRound (sipRound (sipRound k'))) st'
    | otherwise = runRounds d k' st'
  where k' Sip{..} = k $! v0 `xor` v1 `xor` v2 `xor` v3
        st'        = st{ v2 = v2 `xor` 0xff }
{-# INLINE finalize #-}

hashByteString :: Int -> Int -> Word64 -> Word64 -> ByteString -> Word64
hashByteString !c !d k0 k1 (PS fp off len) =
  inlinePerformIO . withForeignPtr fp $ \basePtr ->
    let ptr0 = basePtr `plusPtr` off
        scant = len .&. 7
        endBlocks = ptr0 `plusPtr` (len - scant)
        go !ptr st
            | ptr == endBlocks = readLast ptr
            | otherwise = do
                m <- peekLE64 ptr
                fullBlock c m (go (ptr `plusPtr` 8)) st
          where
            zero !m _ _ = lastBlock c len (LE64 m) (finalize d return) st
            one k m p s = do
              w <- fromIntegral `fmap` peekByte p
              k (m .|. (w `unsafeShiftL` s)) (p `plusPtr` 1) (s+8)
            readLast p =
              case scant of
                0 -> zero 0 p (0::Int)
                1 -> one zero 0 p 0
                2 -> one (one zero) 0 p 0
                3 -> one (one (one zero)) 0 p 0
                4 -> one (one (one (one zero))) 0 p 0
                5 -> one (one (one (one (one zero)))) 0 p 0
                6 -> one (one (one (one (one (one zero))))) 0 p 0
                _ -> one (one (one (one (one (one (one zero)))))) 0 p 0
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

#if !(MIN_VERSION_base(4,5,0))
unsafeShiftL :: Word64 -> Int -> Word64
unsafeShiftL = shiftL
#endif
