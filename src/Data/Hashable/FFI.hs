{-# LANGUAGE CApiFFI          #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE Trustworthy      #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Data.Hashable.FFI (
    -- * One shot
    unsafe_xxh3_64bit_withSeed_ptr,
    unsafe_xxh3_64bit_withSeed_ba,
    unsafe_xxh3_64bit_withSeed_u64,
    unsafe_xxh3_64bit_withSeed_u32,
    -- * Incremental
    unsafe_xxh3_sizeof_state,
    unsafe_xxh3_initState,
    unsafe_xxh3_64bit_reset_withSeed,
    unsafe_xxh3_64bit_digest,
    unsafe_xxh3_64bit_update_ptr,
    unsafe_xxh3_64bit_update_ba,
    unsafe_xxh3_64bit_update_u64,
    unsafe_xxh3_64bit_update_u32,
) where

import Data.Word       (Word32, Word64, Word8)
import Foreign.C.Types (CSize (..))
import Foreign.Ptr     (Ptr)
import GHC.Exts        (ByteArray#, MutableByteArray#)

-- Note: we use unsafe FFI calls, as we expect our use case to be hashing only small data (<1kb, at most 4k).

-------------------------------------------------------------------------------
-- OneShot
-------------------------------------------------------------------------------

foreign import capi unsafe "HsXXHash.h XXH3_64bits_withSeed"
    unsafe_xxh3_64bit_withSeed_ptr :: Ptr Word8 -> CSize -> Word64 -> IO Word64

foreign import capi unsafe "HsXXHash.h hs_XXH3_64bits_withSeed_offset"
    unsafe_xxh3_64bit_withSeed_ba :: ByteArray# -> CSize -> CSize -> Word64 -> Word64

foreign import capi unsafe "HsXXHash.h hs_XXH3_64bits_withSeed_u64"
    unsafe_xxh3_64bit_withSeed_u64 :: Word64 -> Word64 -> Word64

foreign import capi unsafe "HsXXHash.h hs_XXH3_64bits_withSeed_u32"
    unsafe_xxh3_64bit_withSeed_u32 :: Word32 -> Word64 -> Word64

-------------------------------------------------------------------------------
-- Incremental
-------------------------------------------------------------------------------

-- reset and update functions return OK/Error
-- we ignore that:
-- * reset errors only on NULL state
-- * update cannot even error

foreign import capi unsafe "HsXXHash.h value hs_XXH3_sizeof_state_s"
    unsafe_xxh3_sizeof_state :: Int

foreign import capi unsafe "HsXXHash.h XXH3_INITSTATE"
    unsafe_xxh3_initState :: MutableByteArray# s -> IO ()

foreign import capi unsafe "HsXXHash.h XXH3_64bits_reset_withSeed"
    unsafe_xxh3_64bit_reset_withSeed :: MutableByteArray# s -> Word64 -> IO ()

foreign import capi unsafe "HsXXHash.h XXH3_64bits_digest"
    unsafe_xxh3_64bit_digest :: MutableByteArray# s -> IO Word64

foreign import capi unsafe "HsXXHash.h XXH3_64bits_update"
    unsafe_xxh3_64bit_update_ptr :: MutableByteArray# s -> Ptr Word8 -> CSize -> IO ()

foreign import capi unsafe "HsXXHash.h hs_XXH3_64bits_update_offset"
    unsafe_xxh3_64bit_update_ba :: MutableByteArray# s -> ByteArray# -> CSize -> CSize -> IO ()

foreign import capi unsafe "HsXXHash.h hs_XXH3_64bits_update_u64"
    unsafe_xxh3_64bit_update_u64 :: MutableByteArray# s -> Word64 -> IO ()

foreign import capi unsafe "HsXXHash.h hs_XXH3_64bits_update_u32"
    unsafe_xxh3_64bit_update_u32 :: MutableByteArray# s -> Word32 -> IO ()
