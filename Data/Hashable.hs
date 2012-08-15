{-# LANGUAGE BangPatterns, CPP, ForeignFunctionInterface, MagicHash,
             UnliftedFFITypes #-}

------------------------------------------------------------------------
-- |
-- Module      :  Data.Hash
-- Copyright   :  (c) Milan Straka 2010
--                (c) Johan Tibell 2011
--                (c) Bryan O'Sullivan 2011
-- License     :  BSD-style
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module defines a class, 'Hashable', for types that can be
-- converted to a hash value.  This class exists for the benefit of
-- hashing-based data structures.  The module provides instances for
-- basic types and a way to combine hash values.
--
-- The 'hash' function should be as collision-free as possible, which
-- means that the 'hash' function must map the inputs to the hash
-- values as evenly as possible.

module Data.Hashable
    (
      -- * Computing hash values
      Hashable(..)

      -- * Creating new instances
      -- $blocks
    , hashPtr
    , hashPtrWithSalt
#if defined(__GLASGOW_HASKELL__)
    , hashByteArray
    , hashByteArrayWithSalt
#endif
    , combine
    ) where

import Control.Exception (assert)
import Data.Bits (bitSize, shiftL, shiftR, xor)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Data.List (foldl')
import Data.Ratio (Ratio, denominator, numerator)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as BL
#if !MIN_VERSION_bytestring(0,10,0)
import qualified Data.ByteString.Lazy.Internal as BL  -- foldlChunks
#endif
#if defined(__GLASGOW_HASKELL__)
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import qualified Data.Text.Internal as T
import qualified Data.Text.Lazy as LT
#endif
import Foreign.C (CString)
#if __GLASGOW_HASKELL__ >= 703
import Foreign.C (CLong(..))
#else
import Foreign.C (CLong)
#endif
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (alignment, peek, sizeOf)
import System.IO.Unsafe (unsafePerformIO)

-- Byte arrays and Integers.
#if defined(__GLASGOW_HASKELL__)
import GHC.Base (ByteArray#)
# ifdef VERSION_integer_gmp
import GHC.Exts (Int(..))
import GHC.Integer.GMP.Internals (Integer(..))
# endif
#endif

-- ThreadId
#if defined(__GLASGOW_HASKELL__)
import GHC.Conc (ThreadId(..))
import GHC.Prim (ThreadId#)
# if __GLASGOW_HASKELL__ >= 703
import Foreign.C.Types (CInt(..))
# else
import Foreign.C.Types (CInt)
# endif
#else
import Control.Concurrent (ThreadId)
#endif

#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
import System.Mem.StableName
#endif

import Data.Typeable
#if __GLASGOW_HASKELL__ >= 702
import GHC.Fingerprint.Type(Fingerprint(..))
import Data.Typeable.Internal(TypeRep(..))
#endif

#include "MachDeps.h"

infixl 0 `combine`, `hashWithSalt`

------------------------------------------------------------------------
-- * Computing hash values

-- | A default salt used in the default implementation of 'hashWithSalt'.
-- It is specified by FNV-1 hash as a default salt for hashing string like
-- types.
defaultSalt :: Int
defaultSalt = 2166136261
{-# INLINE defaultSalt #-}

-- | The class of types that can be converted to a hash value.
--
-- Minimal implementation: 'hash' or 'hashWithSalt'.
class Hashable a where
    -- | Return a hash value for the argument.
    --
    -- The general contract of 'hash' is:
    --
    --  * This integer need not remain consistent from one execution
    --    of an application to another execution of the same
    --    application.
    --
    --  * If two values are equal according to the '==' method, then
    --    applying the 'hash' method on each of the two values must
    --    produce the same integer result.
    --
    --  * It is /not/ required that if two values are unequal
    --    according to the '==' method, then applying the 'hash'
    --    method on each of the two values must produce distinct
    --    integer results.  However, the programmer should be aware
    --    that producing distinct integer results for unequal values
    --    may improve the performance of hashing-based data
    --    structures.
    hash :: a -> Int
    hash = hashWithSalt defaultSalt

    -- | Return a hash value for the argument, using the given salt.
    --
    -- This method can be used to compute different hash values for
    -- the same input by providing a different salt in each
    -- application of the method.
    --
    -- The contract for 'hashWithSalt' is the same as for 'hash', with
    -- the additional requirement that any instance that defines
    -- 'hashWithSalt' must make use of the salt in its implementation.
    hashWithSalt :: Int -> a -> Int
    hashWithSalt salt x = salt `combine` hash x

instance Hashable () where hash _ = 0

instance Hashable Bool where hash x = case x of { True -> 1; False -> 0 }

instance Hashable Int where hash = id
instance Hashable Int8 where hash = fromIntegral
instance Hashable Int16 where hash = fromIntegral
instance Hashable Int32 where hash = fromIntegral
instance Hashable Int64 where
    hash n
        | bitSize (undefined :: Int) == 64 = fromIntegral n
        | otherwise = fromIntegral (fromIntegral n `xor`
                                   (fromIntegral n `shiftR` 32 :: Word64))

instance Hashable Word where hash = fromIntegral
instance Hashable Word8 where hash = fromIntegral
instance Hashable Word16 where hash = fromIntegral
instance Hashable Word32 where hash = fromIntegral
instance Hashable Word64 where
    hash n
        | bitSize (undefined :: Int) == 64 = fromIntegral n
        | otherwise = fromIntegral (n `xor` (n `shiftR` 32))

instance Hashable Integer where
#if defined(__GLASGOW_HASKELL__) && defined(VERSION_integer_gmp)
    hash (S# int) = I# int
    hash n@(J# size byteArray) | n >= fromIntegral (minBound :: Int) && n <= fromIntegral (maxBound :: Int) = fromInteger n
                               | otherwise = hashByteArrayWithSalt byteArray 0 (SIZEOF_HSWORD * (I# size)) 0

    hashWithSalt salt (S# int) = salt `combine` I# int
    hashWithSalt salt n@(J# size byteArray) | n >= fromIntegral (minBound :: Int) && n <= fromIntegral (maxBound :: Int) = salt `combine` fromInteger n
                                            | otherwise = hashByteArrayWithSalt byteArray 0 (SIZEOF_HSWORD * (I# size)) salt
#else
    hashWithSalt salt = foldl' hashWithSalt salt . go
      where
        go n | inBounds n = [fromIntegral n :: Int]
             | otherwise   = fromIntegral n : go (n `shiftR` WORD_SIZE_IN_BITS)
        maxInt = fromIntegral (maxBound :: Int)
        inBounds x = x >= fromIntegral (minBound :: Int) && x <= maxInt
#endif

instance (Integral a, Hashable a) => Hashable (Ratio a) where
    {-# SPECIALIZE instance Hashable (Ratio Integer) #-}
    hash a = hash (numerator a) `hashWithSalt` denominator a
    hashWithSalt s a = s `hashWithSalt` numerator a `hashWithSalt` denominator a

instance Hashable Float where
    hash x
        | isIEEE x =
            assert (sizeOf x >= sizeOf (0::Word32) &&
                    alignment x >= alignment (0::Word32)) $
            hash ((unsafePerformIO $ with x $ peek . castPtr) :: Word32)
        | otherwise = hash (show x)

instance Hashable Double where
    hash x
        | isIEEE x =
            assert (sizeOf x >= sizeOf (0::Word64) &&
                    alignment x >= alignment (0::Word64)) $
            hash ((unsafePerformIO $ with x $ peek . castPtr) :: Word64)
        | otherwise = hash (show x)

instance Hashable Char where hash = fromEnum

-- | A value with bit pattern (01)* (or 5* in hexa), for any size of Int.
-- It is used as data constructor distinguisher. GHC computes its value during
-- compilation.
distinguisher :: Int
distinguisher = fromIntegral $ (maxBound :: Word) `quot` 3
{-# INLINE distinguisher #-}

instance Hashable a => Hashable (Maybe a) where
    hash Nothing = 0
    hash (Just a) = distinguisher `hashWithSalt` a
    hashWithSalt s Nothing = s `combine` 0
    hashWithSalt s (Just a) = s `combine` distinguisher `hashWithSalt` a

instance (Hashable a, Hashable b) => Hashable (Either a b) where
    hash (Left a)  = 0 `hashWithSalt` a
    hash (Right b) = distinguisher `hashWithSalt` b
    hashWithSalt s (Left a)  = s `combine` 0 `hashWithSalt` a
    hashWithSalt s (Right b) = s `combine` distinguisher `hashWithSalt` b

instance (Hashable a1, Hashable a2) => Hashable (a1, a2) where
    hash (a1, a2) = hash a1 `hashWithSalt` a2
    hashWithSalt s (a1, a2) = s `hashWithSalt` a1 `hashWithSalt` a2

instance (Hashable a1, Hashable a2, Hashable a3) => Hashable (a1, a2, a3) where
    hash (a1, a2, a3) = hash a1 `hashWithSalt` a2 `hashWithSalt` a3
    hashWithSalt s (a1, a2, a3) = s `hashWithSalt` a1 `hashWithSalt` a2
                        `hashWithSalt` a3

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4) =>
         Hashable (a1, a2, a3, a4) where
    hash (a1, a2, a3, a4) = hash a1 `hashWithSalt` a2
                            `hashWithSalt` a3 `hashWithSalt` a4
    hashWithSalt s (a1, a2, a3, a4) = s `hashWithSalt` a1 `hashWithSalt` a2
                            `hashWithSalt` a3 `hashWithSalt` a4

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5)
      => Hashable (a1, a2, a3, a4, a5) where
    hash (a1, a2, a3, a4, a5) =
        hash a1 `hashWithSalt` a2 `hashWithSalt` a3
        `hashWithSalt` a4 `hashWithSalt` a5
    hashWithSalt s (a1, a2, a3, a4, a5) =
        s `hashWithSalt` a1 `hashWithSalt` a2 `hashWithSalt` a3
        `hashWithSalt` a4 `hashWithSalt` a5

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5,
          Hashable a6) => Hashable (a1, a2, a3, a4, a5, a6) where
    hash (a1, a2, a3, a4, a5, a6) =
        hash a1 `hashWithSalt` a2 `hashWithSalt` a3
        `hashWithSalt` a4 `hashWithSalt` a5 `hashWithSalt` a6
    hashWithSalt s (a1, a2, a3, a4, a5, a6) =
        s `hashWithSalt` a1 `hashWithSalt` a2 `hashWithSalt` a3
        `hashWithSalt` a4 `hashWithSalt` a5 `hashWithSalt` a6

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5,
          Hashable a6, Hashable a7) =>
         Hashable (a1, a2, a3, a4, a5, a6, a7) where
    hash (a1, a2, a3, a4, a5, a6, a7) =
        hash a1 `hashWithSalt` a2 `hashWithSalt` a3
        `hashWithSalt` a4 `hashWithSalt` a5 `hashWithSalt` a6 `hashWithSalt` a7
    hashWithSalt s (a1, a2, a3, a4, a5, a6, a7) =
        s `hashWithSalt` a1 `hashWithSalt` a2 `hashWithSalt` a3
        `hashWithSalt` a4 `hashWithSalt` a5 `hashWithSalt` a6 `hashWithSalt` a7

#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
instance Hashable (StableName a) where
    hash = hashStableName
#endif

instance Hashable a => Hashable [a] where
    {-# SPECIALIZE instance Hashable [Char] #-}
    hashWithSalt = foldl' hashWithSalt

instance Hashable B.ByteString where
    hashWithSalt salt bs = B.inlinePerformIO $
                           B.unsafeUseAsCStringLen bs $ \(p, len) ->
                           hashPtrWithSalt p (fromIntegral len) salt

instance Hashable BL.ByteString where
    hashWithSalt salt = BL.foldlChunks hashWithSalt salt

#if defined(__GLASGOW_HASKELL__)
instance Hashable T.Text where
    hashWithSalt salt (T.Text arr off len) =
        hashByteArrayWithSalt (TA.aBA arr) (off `shiftL` 1) (len `shiftL` 1)
        salt

instance Hashable LT.Text where
    hashWithSalt salt = LT.foldlChunks hashWithSalt salt
#endif


-- | Compute the hash of a ThreadId.  For GHC, we happen to know a
-- trick to make this fast.
hashThreadId :: ThreadId -> Int
{-# INLINE hashThreadId #-}
#if defined(__GLASGOW_HASKELL__)
hashThreadId (ThreadId t) = hash (fromIntegral (getThreadId t) :: Int)
foreign import ccall unsafe "rts_getThreadId" getThreadId :: ThreadId# -> CInt
#else
hashThreadId = hash . show
#endif

instance Hashable ThreadId where
    hash = hashThreadId
    {-# INLINE hash #-}


-- | Compute the hash of a TypeRep, in various GHC versions we can do this quickly.
hashTypeRep :: TypeRep -> Int
{-# INLINE hashTypeRep #-}
#if __GLASGOW_HASKELL__ >= 702
-- Fingerprint is just the MD5, so taking any Int from it is fine
hashTypeRep (TypeRep (Fingerprint x _) _ _) = fromIntegral x
#elif __GLASGOW_HASKELL__ >= 606
hashTypeRep = B.inlinePerformIO . typeRepKey
#else
hashTypeRep = hash . show
#endif

instance Hashable TypeRep where
    hash = hashTypeRep
    {-# INLINE hash #-}


------------------------------------------------------------------------
-- * Creating new instances

-- $blocks
--
-- The functions below can be used when creating new instances of
-- 'Hashable'.  For example, for many string-like types the
-- 'hashWithSalt' method can be defined in terms of either
-- 'hashPtrWithSalt' or 'hashByteArrayWithSalt'.  Here's how you could
-- implement an instance for the 'B.ByteString' data type, from the
-- @bytestring@ package:
--
-- > import qualified Data.ByteString as B
-- > import qualified Data.ByteString.Internal as B
-- > import qualified Data.ByteString.Unsafe as B
-- > import Data.Hashable
-- > import Foreign.Ptr (castPtr)
-- >
-- > instance Hashable B.ByteString where
-- >     hashWithSalt salt bs = B.inlinePerformIO $
-- >                            B.unsafeUseAsCStringLen bs $ \(p, len) ->
-- >                            hashPtrWithSalt p (fromIntegral len) salt
--
-- Use 'hashWithSalt' to create a hash value from several values,
-- using this recipe:
--
-- > instance (Hashable a1, Hashable a2) => Hashable (a1, a2) where
-- >     hash (a1, a2) = hash a1 `hashWithSalt` a2
--
-- You can combine multiple hash values using 'combine', using this
-- recipe:
--
-- > combineTwo h1 h2 = 17 `combine` h1 `combine` h2
--
-- As zero is a left identity of 'combine', a nonzero "seed" is used
-- so that the number of combined hash values affects the final
-- result, even if the first hash values are zero.  The value 17 is
-- arbitrary.
--
-- When possible, use 'hashWithSalt' to compute a hash value from
-- multiple values instead of computing separate hashes for each value
-- and then combining them using 'combine'.

-- | Compute a hash value for the content of this pointer.
hashPtr :: Ptr a      -- ^ pointer to the data to hash
        -> Int        -- ^ length, in bytes
        -> IO Int     -- ^ hash value
hashPtr p len = hashPtrWithSalt p len defaultSalt

-- | Compute a hash value for the content of this pointer, using an
-- initial salt.
--
-- This function can for example be used to hash non-contiguous
-- segments of memory as if they were one contiguous segment, by using
-- the output of one hash as the salt for the next.
hashPtrWithSalt :: Ptr a   -- ^ pointer to the data to hash
                -> Int     -- ^ length, in bytes
                -> Int     -- ^ salt
                -> IO Int  -- ^ hash value
hashPtrWithSalt p len salt =
    fromIntegral `fmap` c_hashCString (castPtr p) (fromIntegral len)
    (fromIntegral salt)

foreign import ccall unsafe "hashable_fnv_hash" c_hashCString
    :: CString -> CLong -> CLong -> IO CLong

#if defined(__GLASGOW_HASKELL__)
-- | Compute a hash value for the content of this 'ByteArray#',
-- beginning at the specified offset, using specified number of bytes.
-- Availability: GHC.
hashByteArray :: ByteArray#  -- ^ data to hash
              -> Int         -- ^ offset, in bytes
              -> Int         -- ^ length, in bytes
              -> Int         -- ^ hash value
hashByteArray ba0 off len = hashByteArrayWithSalt ba0 off len defaultSalt
{-# INLINE hashByteArray #-}

-- | Compute a hash value for the content of this 'ByteArray#', using
-- an initial salt.
--
-- This function can for example be used to hash non-contiguous
-- segments of memory as if they were one contiguous segment, by using
-- the output of one hash as the salt for the next.
--
-- Availability: GHC.
hashByteArrayWithSalt
    :: ByteArray#  -- ^ data to hash
    -> Int         -- ^ offset, in bytes
    -> Int         -- ^ length, in bytes
    -> Int         -- ^ salt
    -> Int         -- ^ hash value
hashByteArrayWithSalt ba !off !len !h0 =
    fromIntegral $ c_hashByteArray ba (fromIntegral off) (fromIntegral len)
    (fromIntegral h0)

foreign import ccall unsafe "hashable_fnv_hash_offset" c_hashByteArray
    :: ByteArray# -> CLong -> CLong -> CLong -> CLong
#endif

-- | Combine two given hash values.  'combine' has zero as a left
-- identity.
combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2
