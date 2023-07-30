{-# LANGUAGE CPP, BangPatterns, MagicHash, CApiFFI, UnliftedFFITypes, PatternSynonyms #-}
{-# LANGUAGE Trustworthy #-}
-- | A module containing low-level hash primitives.
module Data.Hashable.LowLevel (
    Salt,
    pattern MkSalt,
    finaliseHash,
    defaultSalt,
    hashInt,
    hashInt64,
    hashWord64,
    hashPtrWithSalt,
    hashByteArrayWithSalt,
) where

#include "MachDeps.h"

import Foreign.C (CString)
import Foreign.Ptr (Ptr, castPtr)
import GHC.Base (ByteArray#)

#ifdef HASHABLE_RANDOM_SEED
import System.IO.Unsafe (unsafePerformIO)
#endif

import Data.Hashable.Imports

-------------------------------------------------------------------------------
-- Initial seed
-------------------------------------------------------------------------------

type Hash = Int

-- | The hash state.
-- For historical reasons it is called salt.
type Salt = Int

pattern MkSalt :: Int -> Salt
pattern MkSalt x = x
{-# COMPLETE MkSalt #-}

#ifdef HASHABLE_RANDOM_SEED
initialSeed :: Word64
initialSeed = unsafePerformIO initialSeedC
{-# NOINLINE initialSeed #-}

foreign import capi "HsHashable.h hs_hashable_init" initialSeedC :: IO Word64
#endif

-- | A default salt used in the implementation of 'hash'.
defaultSalt :: Salt
#ifdef HASHABLE_RANDOM_SEED
defaultSalt = hashInt defaultSalt' (fromIntegral initialSeed)
#else
defaultSalt = defaultSalt'
#endif
{-# INLINE defaultSalt #-}

defaultSalt' :: Salt
#if WORD_SIZE_IN_BITS == 64
defaultSalt' = MkSalt (-3750763034362895579) -- 14695981039346656037 :: Int64
#else
defaultSalt' = MkSalt (-2128831035) -- 2166136261 :: Int32
#endif
{-# INLINE defaultSalt' #-}

-------------------------------------------------------------------------------
-- Hash primitives
-------------------------------------------------------------------------------

-- | Extract final hash value from the hash state 'Salt'.
finaliseHash :: Salt -> Hash
finaliseHash (MkSalt s) = s

-- | Hash 'Int'. First argument is a salt, second argument is an 'Int'.
-- The result is new salt / hash value.
hashInt :: Salt -> Int -> Salt
hashInt (MkSalt s) x = MkSalt $ s `rnd` x1 `rnd` x2 `rnd` x3 `rnd` x4
  where
    {-# INLINE rnd #-}
    {-# INLINE x1 #-}
    {-# INLINE x2 #-}
    {-# INLINE x3 #-}
    {-# INLINE x4 #-}
#if WORD_SIZE_IN_BITS == 64
    -- See https://github.com/haskell-unordered-containers/hashable/issues/270
    -- FNV-1 is defined to hash byte at the time.
    -- We used to hash whole Int at once, which provided very bad mixing.
    -- Current is a performance-quality compromise, we do four rounds per Int (instead of 8 for FNV-1 or 1 for previous hashable).
    rnd a b = (a * 1099511628211) `xor` b
    x1 = shiftR x 48 .&. 0xffff
    x2 = shiftR x 32 .&. 0xffff
    x3 = shiftR x 16 .&. 0xffff
    x4 =           x .&. 0xffff
#else
    rnd a b = (a * 16777619) `xor` b
    x1 = shiftR x 24 .&. 0xff
    x2 = shiftR x 16 .&. 0xff
    x3 = shiftR x  8 .&. 0xff
    x4 =           x .&. 0xff
#endif

-- Note: FNV-1 hash takes a byte of data at once, here we take an 'Int',
-- which is 4 or 8 bytes. Whether that's bad or not, I don't know.

hashInt64  :: Salt -> Int64 -> Salt
hashWord64 :: Salt -> Word64 -> Salt

#if WORD_SIZE_IN_BITS == 64
hashInt64  s x = hashInt s (fromIntegral x)
hashWord64 s x = hashInt s (fromIntegral x)
#else
hashInt64  s x = hashInt (hashInt s (fromIntegral x)) (fromIntegral (x `shiftR` 32))
hashWord64 s x = hashInt (hashInt s (fromIntegral x)) (fromIntegral (x `shiftR` 32))
#endif

-- | Compute a hash value for the content of this pointer, using an
-- initial salt.
--
-- This function can for example be used to hash non-contiguous
-- segments of memory as if they were one contiguous segment, by using
-- the output of one hash as the salt for the next.
hashPtrWithSalt :: Ptr a   -- ^ pointer to the data to hash
                -> Int     -- ^ length, in bytes
                -> Salt    -- ^ salt
                -> IO Salt -- ^ hash value
hashPtrWithSalt p len (MkSalt salt) =
    (MkSalt . fromIntegral) `fmap` c_hashCString (castPtr p) (fromIntegral len)
    (fromIntegral salt)

-- | Compute a hash value for the content of this 'ByteArray#', using
-- an initial salt.
--
-- This function can for example be used to hash non-contiguous
-- segments of memory as if they were one contiguous segment, by using
-- the output of one hash as the salt for the next.
hashByteArrayWithSalt
    :: ByteArray#  -- ^ data to hash
    -> Int         -- ^ offset, in bytes
    -> Int         -- ^ length, in bytes
    -> Salt        -- ^ salt
    -> Salt        -- ^ hash value
hashByteArrayWithSalt ba !off !len !(MkSalt h) =
    MkSalt $ fromIntegral $ c_hashByteArray ba (fromIntegral off) (fromIntegral len)
    (fromIntegral h)

foreign import capi unsafe "HsHashable.h hashable_fnv_hash" c_hashCString
#if WORD_SIZE_IN_BITS == 64
    :: CString -> Int64 -> Int64 -> IO Word64
#else
    :: CString -> Int32 -> Int32 -> IO Word32
#endif

#if __GLASGOW_HASKELL__ >= 802
foreign import capi unsafe "HsHashable.h hashable_fnv_hash_offset" c_hashByteArray
#else
foreign import ccall unsafe "hashable_fnv_hash_offset" c_hashByteArray
#endif
#if WORD_SIZE_IN_BITS == 64
    :: ByteArray# -> Int64 -> Int64 -> Int64 -> Word64
#else
    :: ByteArray# -> Int32 -> Int32 -> Int32 -> Word32
#endif
