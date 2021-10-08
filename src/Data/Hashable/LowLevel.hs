{-# LANGUAGE CPP, BangPatterns, MagicHash, CApiFFI, UnliftedFFITypes #-}
{-# LANGUAGE Trustworthy, RankNTypes #-}
-- | A module containing low-level hash primitives.
module Data.Hashable.LowLevel (
    Salt,
    defaultSalt,
    hashInt,
    hashInt64,
    hashWord64,
    hashPtrWithSalt,
    hashByteArrayWithSalt,
    k0, -- TODO remove
    k1,
    withState,
    hashPtrChunk,
    siphash
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
#if WORD_SIZE_IN_BITS != 64
import Data.Bits (shiftR)
#endif
#if __GLASGOW_HASKELL__ >= 703
import Foreign.C (CSize(..))
#else
import Foreign.C (CSize)
#endif
#if __GLASGOW_HASKELL__ <= 784
import Control.Applicative((<$>), (<$))
#endif


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
    withState k0 k1 (\state -> salt <$ hashPtrChunk p len state)

-- TODO don't hardcode these! The entire point is that the user can determine
--  or hide k0 & k1 (both making up k)
k0 :: Word64
k0 = 0x56e2b8a0aee1721a
{-# INLINE k0 #-}

fromSalt :: Int -> Word64
#if WORD_SIZE_IN_BITS == 64
fromSalt = fromIntegral
#else
fromSalt v = fromIntegral v `xor` k1
#endif

-- TODO this should be hideable, see 'k0'
k1 :: Word64
k1 = 0x7654954208bdfef9
{-# INLINE k1 #-}

newtype SipHashState = MkSipHashState { unstate ::  Ptr Word64 }

siphash
  :: CInt -- ^ compression rounds
  -> CInt -- ^ finalize rounds
  -> Word64 -- ^ k0
  -> Word64 -- ^ k1
  -> Ptr Word8 -- ^ data
  -> CSize -- ^ length of data
  -> Word64 -- ^ hash
siphash finalizeRounds compression k0 k1 data' length = fromIntegral $ unsafePerformIO $
  withState' finalizeRounds k0 k1 $ \state ->
    (0 <$ hashPtrChunk' compression data' length state)

-- | allocates a siphash state for given k0.
--   this allows usage of 'hashByteArrayChunck'
--   after those calls are made, the hash will be returned
withState :: Word64 -- ^ k0 (k for key, should be secret)
          -> Word64 -- ^ k1 (second part of the key)
          -> (SipHashState -> IO Salt
            ) -- ^ the function to mutate the sipState, should return a salt,
              -- a salt of 0 means no salting
              -- (the xor doesn't do anything with that value).
          -> IO Int -- ^ the hash value
withState = withState' defFinalizeRounds

withState' :: CInt -- ^ finalize rounds
          -> Word64
          -> Word64
          -> (SipHashState -> IO Salt)
          -> IO Int
withState' finalizeRounds k0 k1 fun =
  allocaArray 4 $ \v -> do
    c_siphash_init k0 k1 v
    salt <- fun $ MkSipHashState v
    hashInt salt -- just jam the salt in somewhere (k0, or k1 could've worked as well)
      . fromIntegral <$> c_siphash_finalize finalizeRounds v

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
hashByteArrayWithSalt ba !off !len !salt =
    unsafePerformIO $ withState k0 k1 (\state -> salt <$ hashByteArrayChunck ba off len state)

-- | Sip hash is streamable after initilization. Use 'withState'
--   to obtain 'SipHashState x'
hashByteArrayChunck
    :: ByteArray#  -- ^ data to hash
    -> Int         -- ^ offset, in bytes
    -> Int         -- ^ length, in bytes
    -> SipHashState -- ^ this mutates (out var)
    -> IO ()
hashByteArrayChunck ba off len (MkSipHashState v) =
  c_siphash_compression_offset defCompressionRounds v ba (fromIntegral off) (fromIntegral len)

hashPtrChunk
    :: Ptr a  -- ^ data to hash
    -> Int         -- ^ length, in bytes
    -> SipHashState -- ^ this mutates (out var)
    -> IO ()
hashPtrChunk data' length = hashPtrChunk' defCompressionRounds data' (fromIntegral length)

hashPtrChunk'
    :: CInt -- ^ compression rounds
    -> Ptr a
    -> CSize
    -> SipHashState
    -> IO ()
hashPtrChunk' compressions ba len (MkSipHashState v) =
  c_siphash_compression_words compressions v (castPtr ba) 0 len

#if __GLASGOW_HASKELL__ >= 802
foreign import capi unsafe "siphash.h hashable_siphash_finalize" c_siphash_finalize
#else
foreign import ccall unsafe "hashable_siphash_finalize" c_siphash_finalize
#endif
    :: CInt -> Ptr Word64 -> IO Word64

#if __GLASGOW_HASKELL__ >= 802
foreign import capi unsafe "siphash.h hashable_siphash_compression" c_siphash_compression_offset
#else
foreign import ccall unsafe "hashable_siphash_compression"
        c_siphash_compression_offset
#endif
    :: CInt -> Ptr Word64 -> ByteArray# -> CSize -> CSize -> IO ()

-- I don't think it's easy to convince the compiler  ByteArray# ~ Ptr Word8
-- (I'm probably wrong on this even, but this is what the previous version basically did,
-- and I (naivily probably) believe them)
-- so we just add another binding to the same function
#if __GLASGOW_HASKELL__ >= 802
foreign import capi unsafe "siphash.h hashable_siphash_compression" c_siphash_compression_words
#else
foreign import ccall unsafe "hashable_siphash_compression"
        c_siphash_compression_words
#endif
    :: CInt -> Ptr Word64 -> Ptr Word8 -> CSize -> CSize -> IO ()

#if __GLASGOW_HASKELL__ >= 802
foreign import capi unsafe "siphash.h hashable_siphash_init" c_siphash_init
#else
foreign import ccall unsafe "hashable_siphash_init" c_siphash_init
#endif
    :: Word64 -> Word64 -> Ptr Word64 -> IO ()


defCompressionRounds :: CInt
defCompressionRounds = 2

defFinalizeRounds :: CInt
defFinalizeRounds = 4
