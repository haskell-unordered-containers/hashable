{-# LANGUAGE CPP, BangPatterns, MagicHash, CApiFFI, UnliftedFFITypes #-}
{-# LANGUAGE Trustworthy #-}
-- | A module containing low-level hash primitives.
module Data.Hashable.LowLevel (
    Salt,
    defaultSalt,
    hashInt,
    hashInt64,
    hashWord64,
    hashPtrWithSalt,
    hashByteArrayWithSalt,
    hashLazyTextWithSalt,
    hashLazyByteStringWithSalt
) where

#include "MachDeps.h"

import Foreign.C (CString)
import Foreign.Ptr (Ptr, castPtr)
import GHC.Base (ByteArray#)
import Foreign.C.Types (CInt(..))
import qualified Data.Text.Array as TA
import qualified Data.Text.Internal as T
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Internal.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as BL
import Foreign.Marshal.Array(advancePtr, allocaArray)
import System.IO.Unsafe(unsafePerformIO)
import Foreign.Storable (alignment, peek, sizeOf)
import Foreign.Ptr(nullPtr)
import Data.Bits (shiftL, shiftR, xor)
#if (MIN_VERSION_bytestring(0,10,0))
import qualified Data.ByteString.Lazy.Internal as BL  -- foldlChunks
#endif

#ifdef HASHABLE_RANDOM_SEED
import System.IO.Unsafe (unsafePerformIO)
#endif

import Data.Hashable.Imports

-------------------------------------------------------------------------------
-- Initial seed
-------------------------------------------------------------------------------

type Salt = Int

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
defaultSalt' = -3750763034362895579 -- 14695981039346656037 :: Int64
#else
defaultSalt' = -2128831035 -- 2166136261 :: Int32
#endif
{-# INLINE defaultSalt' #-}

-------------------------------------------------------------------------------
-- Hash primitives
-------------------------------------------------------------------------------

-- | Hash 'Int'. First argument is a salt, second argument is an 'Int'.
-- The result is new salt / hash value.
hashInt :: Salt -> Int -> Salt
#if WORD_SIZE_IN_BITS == 64
hashInt s x = (s * 1099511628211) `xor` x
#else
hashInt s x = (s * 16777619) `xor` x
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
hashPtrWithSalt p len salt =
    fromIntegral `fmap` c_siphash24 k0 (fromSalt salt) (castPtr p)
                        (fromIntegral len)

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
hashByteArrayWithSalt ba !off !len !h =
    fromIntegral $
    c_siphash24_offset k0 (fromSalt h) ba (fromIntegral off) (fromIntegral len)

k0 :: Word64
k0 = 0x56e2b8a0aee1721a
{-# INLINE k0 #-}

fromSalt :: Int -> Word64
#if WORD_SIZE_IN_BITS == 64
fromSalt = fromIntegral
#else
fromSalt v = fromIntegral v `xor` k1

k1 :: Word64
k1 = 0x7654954208bdfef9
{-# INLINE k1 #-}
#endif

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

foreign import ccall unsafe "hashable_siphash24_offset" c_siphash24_offset
    :: Word64 -> Word64 -> ByteArray# -> CSize -> CSize -> Word64

foreign import ccall unsafe "hashable_siphash24" c_siphash24
    :: Word64 -> Word64 -> Ptr Word8 -> CSize -> IO Word64


hashLazyByteStringWithSalt :: Int -> BL.ByteString -> Int
hashLazyByteStringWithSalt salt cs0 = unsafePerformIO . allocaArray 5 $ \v -> do
  c_siphash_init k0 (fromSalt salt) v
  let go !buffered !totallen (BL.Chunk c cs) =
        B.unsafeUseAsCStringLen c $ \(ptr, len) -> do
          let len' = fromIntegral len
          buffered' <- c_siphash24_chunk buffered v (castPtr ptr) len' (-1)
          go buffered' (totallen + len') cs
      go buffered totallen _ = do
        _ <- c_siphash24_chunk buffered v nullPtr 0 totallen
        fromIntegral `fmap` peek (v `advancePtr` 4)
  go 0 0 cs0

foreign import ccall unsafe "hashable_siphash24_chunk" c_siphash24_chunk
    :: CInt -> Ptr Word64 -> Ptr Word8 -> CSize -> CSize -> IO CInt

foreign import ccall unsafe "hashable_siphash_init" c_siphash_init
    :: Word64 -> Word64 -> Ptr Word64 -> IO ()

hashLazyTextWithSalt :: Int -> TL.Text -> Int
hashLazyTextWithSalt salt cs0 = unsafePerformIO . allocaArray 5 $ \v -> do
  c_siphash_init k0 (fromSalt salt) v
  let go !buffered !totallen (TL.Chunk (T.Text arr off len) cs) = do
        let len' = fromIntegral (len `shiftL` 1)
        buffered' <- c_siphash24_chunk_offset buffered v (TA.aBA arr)
                     (fromIntegral (off `shiftL` 1)) len' (-1)
        go buffered' (totallen + len') cs
      go buffered totallen _ = do
        _ <- c_siphash24_chunk buffered v nullPtr 0 totallen
        fromIntegral `fmap` peek (v `advancePtr` 4)
  go 0 0 cs0

foreign import ccall unsafe "hashable_siphash24_chunk_offset"
        c_siphash24_chunk_offset
    :: CInt -> Ptr Word64 -> ByteArray# -> CSize -> CSize -> CSize -> IO CInt
