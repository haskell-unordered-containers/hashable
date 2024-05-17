{-# LANGUAGE CPP           #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE Trustworthy   #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.Hashable.Mix (mixHash) where

#include "MachDeps.h"

import Data.Bits (unsafeShiftR, xor)
import GHC.Exts  (Word (..), byteSwap#, timesWord2#, xor#)

mulFold :: Word -> Word -> Word
mulFold (W# x) (W# y) = case timesWord2# x y of
    (# hi, lo #) -> W# (xor# hi lo)

byteSwap :: Word -> Word
byteSwap (W# w) = W# (byteSwap# w)

avalanche :: Word -> Word
avalanche z0 =
#if WORD_SIZE_IN_BITS == 64
   -- MurmurHash3Mixer
    let z1 = shiftXorMultiply 33 0xff51afd7ed558ccd z0
        z2 = shiftXorMultiply 33 0xc4ceb9fe1a85ec53 z1
        z3 = shiftXor 33 z2
    in z3
#else
   -- MurmurHash3Mixer 32bit
    let z1 = shiftXorMultiply 16 0x85ebca6b z0
        z2 = shiftXorMultiply 13 0xc2b2ae35 z1
        z3 = shiftXor 16 z2
    in z3
#endif

shiftXor :: Int -> Word -> Word
shiftXor n w = w `xor` (w `unsafeShiftR` n)

shiftXorMultiply :: Int -> Word -> Word -> Word
shiftXorMultiply n k w = shiftXor n w * k

-- | Mix hash is inspired by how xxh3 works on small (<=16byte) inputs.
mixHash :: Word -> Word -> Word
mixHash hi lo = avalanche (byteSwap lo + hi + mulFold hi lo)
