{-# LANGUAGE BangPatterns, CPP, ForeignFunctionInterface, MagicHash,
             ScopedTypeVariables, UnliftedFFITypes #-}
#ifdef GENERICS
{-# LANGUAGE DefaultSignatures, FlexibleContexts #-}
#endif

------------------------------------------------------------------------
-- |
-- Module      :  Data.Hashable.Class
-- Copyright   :  (c) Milan Straka 2010
--                (c) Johan Tibell 2011
--                (c) Bryan O'Sullivan 2011, 2012
-- License     :  BSD-style
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module defines a class, 'Hashable', for types that can be
-- converted to a hash value.  This class exists for the benefit of
-- hashing-based data structures.  The module provides instances for
-- most standard types.

module Data.Hashable.Class
    (
      -- * Computing hash values
      Hashable(hashWithSalt)
#ifdef GENERICS
      -- ** Support for generics
    , GHashable(..)
#endif
    , hash

      -- * Creating new instances
    , hashUsing
    , hashPtr
    , hashPtrWithSalt
#if defined(__GLASGOW_HASKELL__)
    , hashByteArray
    , hashByteArrayWithSalt
#endif
    ) where

import Control.Exception (assert)
import Data.Bits (shiftL, xor)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Data.List (foldl')
import Data.Ratio (Ratio, denominator, numerator)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BL
#if defined(__GLASGOW_HASKELL__)
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import qualified Data.Text.Internal as T
import qualified Data.Text.Lazy as TL
# ifdef GENERICS
import GHC.Generics
# endif
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
# else
import Data.Bits (shiftR)
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

#ifndef FIXED_SALT
import Control.Exception (tryJust)
import Control.Monad (guard)
import Data.Hashable.RandomSource (getRandomBytes_)
import Foreign.Marshal.Alloc (alloca)
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError)
#endif

#include "MachDeps.h"

infixl 0 `hashWithSalt`

------------------------------------------------------------------------
-- * Computing hash values

-- | A default salt used in the implementation of 'hash'.
--
-- To reduce the probability of hash collisions, the value of the
-- default salt may vary from one program invocation to the next
-- unless this package is compiled with the @fixed-salt@ flag set.
defaultSalt, fixedSalt :: Int

fixedSalt = 0xdc36d1615b7400a4
{-# INLINE fixedSalt #-}

#ifdef FIXED_SALT

defaultSalt = fixedSalt
{-# INLINE defaultSalt #-}

#else

defaultSalt = unsafePerformIO $ do
  let varName = "HASHABLE_SALT"
  msalt <- tryJust (guard . isDoesNotExistError) $ getEnv varName
  case msalt of
    Right "random" -> alloca $ \p -> do
      getRandomBytes_ "defaultSalt" p (sizeOf (undefined :: Int))
      peek p
    Right s -> case reads s of
                 [(salt, "")] -> return salt
                 _            -> fail $ "Fatal: cannot parse contents of " ++
                                        varName ++ " environment variable"
    Left _ -> return fixedSalt
{-# NOINLINE defaultSalt #-}

#endif

-- | The class of types that can be converted to a hash value.
class Hashable a where
    -- | Return a hash value for the argument, using the given salt.
    --
    -- The general contract of 'hashWithSalt' is:
    --
    --  * If a value is hashed using the same salt during distinct
    --    runs of an application, the result must remain the
    --    same. (This is necessary to make it possible to store hashes
    --    on persistent media.)
    --
    --  * If two values are equal according to the '==' method, then
    --    applying the 'hashWithSalt' method on each of the two values
    --    /must/ produce the same integer result if the same salt is
    --    used in each case.
    --
    --  * It is /not/ required that if two values are unequal
    --    according to the '==' method, then applying the
    --    'hashWithSalt' method on each of the two values must produce
    --    distinct integer results.  (Every programmer will be aware
    --    that producing distinct integer results for unequal values
    --    will improve the performance of hashing-based data
    --    structures.)
    --
    -- This method can be used to compute different hash values for
    -- the same input by providing a different salt in each
    -- application of the method. This implies that any instance that
    -- defines 'hashWithSalt' /must/ make use of the salt in its
    -- implementation.
    hashWithSalt :: Int -> a -> Int

    -- | (/Not public/) Hash a list of values. Used to make hashing of
    -- 'String' and 'Word8' more efficient.
    hashListWithSalt :: Int -> [a] -> Int
    hashListWithSalt = foldl' hashWithSalt

#ifdef GENERICS
    default hashWithSalt :: (Generic a, GHashable (Rep a)) => Int -> a -> Int
    hashWithSalt salt = ghashWithSalt salt . from

-- | The class of types that can be generically hashed.
class GHashable f where
    ghashWithSalt :: Int -> f a -> Int
#endif

-- | Return a hash value for the argument. Defined in terms of
-- 'hashWithSalt' and a default salt.
--
-- (See the \"Hashing and security\" section of the
-- "Data.Hashable#security" documentation for an important note on
-- working safely with untrusted user input.)
hash :: Hashable a => a -> Int
hash = hashWithSalt defaultSalt

-- | Transform a value into a 'Hashable' value, then hash the
-- transformed value using the given salt.
--
-- This is a useful shorthand in cases where a type can easily be
-- mapped to another type that is already an instance of 'Hashable'.
-- Example:
--
-- > data Foo = Foo | Bar
-- >          deriving (Enum)
-- >
-- > instance Hashable Foo where
-- >     hashWithSalt = hashUsing fromEnum
hashUsing :: (Hashable b) =>
             (a -> b)           -- ^ Transformation function.
          -> Int                -- ^ Salt.
          -> a                  -- ^ Value to transform.
          -> Int
hashUsing f salt x = hashWithSalt salt (f x)
{-# INLINE hashUsing #-}

instance Hashable Int where hashWithSalt = hashNative
instance Hashable Int16 where hashWithSalt = hashNative
instance Hashable Int32 where hashWithSalt = hashNative
instance Hashable Int64 where hashWithSalt = hash64

instance Hashable Word where hashWithSalt = hashNative
instance Hashable Word16 where hashWithSalt = hashNative
instance Hashable Word32 where hashWithSalt = hashNative
instance Hashable Word64 where hashWithSalt = hash64

instance Hashable () where hashWithSalt = hashUsing fromEnum
instance Hashable Bool where hashWithSalt = hashUsing fromEnum
instance Hashable Ordering where hashWithSalt = hashUsing fromEnum

instance Hashable Int8 where
    hashWithSalt = hashNative
    hashListWithSalt salt = hashUsing B.pack salt . map fromIntegral
    {-# NOINLINE hashListWithSalt #-}

instance Hashable Word8 where
    hashWithSalt = hashNative
    hashListWithSalt = hashUsing B.pack
    {-# NOINLINE hashListWithSalt #-}

instance Hashable Char where
    hashWithSalt = hashUsing fromEnum
    hashListWithSalt = hashUsing T.pack
    {-# NOINLINE hashListWithSalt #-}

-- | Hash an integer of at most the native width supported by the
-- machine.
hashNative :: (Integral a) => Int -> a -> Int
hashNative salt = fromIntegral . go . xor (fromIntegral salt) . fromIntegral
  where
#if WORD_SIZE_IN_BITS == 32
    go :: Word32 -> Word32
#else
    go :: Word64 -> Word64
#endif
    go = id

-- | Hash a 64-bit integer.
hash64 :: (Integral a) => Int -> a -> Int
hash64 salt = fromIntegral . go . xor (fromIntegral salt) . fromIntegral
  where
    go :: Word64 -> Word64
    go = id

instance Hashable Integer where
#if defined(__GLASGOW_HASKELL__) && defined(VERSION_integer_gmp)
    hashWithSalt salt (S# int) = hashWithSalt salt (I# int)
    hashWithSalt salt n@(J# size# byteArray)
        | n >= minInt && n <= maxInt = hashWithSalt salt (fromInteger n :: Int)
        | otherwise = let size = I# size#
                          numBytes = SIZEOF_HSWORD * abs size
                      in hashByteArrayWithSalt byteArray 0 numBytes salt
                         `hashWithSalt` size
      where minInt = fromIntegral (minBound :: Int)
            maxInt = fromIntegral (maxBound :: Int)
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
    hashWithSalt s a = s `hashWithSalt` numerator a `hashWithSalt` denominator a

instance Hashable Float where
    hashWithSalt salt x
        | isIEEE x =
            assert (sizeOf x >= sizeOf (0::Word32) &&
                    alignment x >= alignment (0::Word32)) $
            hashWithSalt salt
              ((unsafePerformIO $ with x $ peek . castPtr) :: Word32)
        | otherwise = hashWithSalt salt (show x)

instance Hashable Double where
    hashWithSalt salt x
        | isIEEE x =
            assert (sizeOf x >= sizeOf (0::Word64) &&
                    alignment x >= alignment (0::Word64)) $
            hashWithSalt salt
              ((unsafePerformIO $ with x $ peek . castPtr) :: Word64)
        | otherwise = hashWithSalt salt (show x)

-- | A value with bit pattern (01)* (or 5* in hexa), for any size of Int.
-- It is used as data constructor distinguisher. GHC computes its value during
-- compilation.
distinguisher :: Int
distinguisher = fromIntegral $ (maxBound :: Word) `quot` 3
{-# INLINE distinguisher #-}

instance Hashable a => Hashable (Maybe a) where
    hashWithSalt s Nothing = hashWithSalt s (0::Int)
    hashWithSalt s (Just a) = hashWithSalt s a `hashWithSalt` distinguisher

instance (Hashable a, Hashable b) => Hashable (Either a b) where
    hashWithSalt s (Left a)  = hashWithSalt s a
    hashWithSalt s (Right b) = hashWithSalt s b `hashWithSalt` distinguisher

instance (Hashable a1, Hashable a2) => Hashable (a1, a2) where
    hashWithSalt s (a1, a2) = s `hashWithSalt` a1 `hashWithSalt` a2

instance (Hashable a1, Hashable a2, Hashable a3) => Hashable (a1, a2, a3) where
    hashWithSalt s (a1, a2, a3) = s `hashWithSalt` a1 `hashWithSalt` a2
                        `hashWithSalt` a3

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4) =>
         Hashable (a1, a2, a3, a4) where
    hashWithSalt s (a1, a2, a3, a4) = s `hashWithSalt` a1 `hashWithSalt` a2
                            `hashWithSalt` a3 `hashWithSalt` a4

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5)
      => Hashable (a1, a2, a3, a4, a5) where
    hashWithSalt s (a1, a2, a3, a4, a5) =
        s `hashWithSalt` a1 `hashWithSalt` a2 `hashWithSalt` a3
        `hashWithSalt` a4 `hashWithSalt` a5

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5,
          Hashable a6) => Hashable (a1, a2, a3, a4, a5, a6) where
    hashWithSalt s (a1, a2, a3, a4, a5, a6) =
        s `hashWithSalt` a1 `hashWithSalt` a2 `hashWithSalt` a3
        `hashWithSalt` a4 `hashWithSalt` a5 `hashWithSalt` a6

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5,
          Hashable a6, Hashable a7) =>
         Hashable (a1, a2, a3, a4, a5, a6, a7) where
    hashWithSalt s (a1, a2, a3, a4, a5, a6, a7) =
        s `hashWithSalt` a1 `hashWithSalt` a2 `hashWithSalt` a3
        `hashWithSalt` a4 `hashWithSalt` a5 `hashWithSalt` a6 `hashWithSalt` a7

#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
instance Hashable (StableName a) where
    hashWithSalt = hashUsing hashStableName
#endif

instance Hashable a => Hashable [a] where
    hashWithSalt = hashListWithSalt

instance Hashable B.ByteString where
    hashWithSalt salt bs = B.inlinePerformIO $
                           B.unsafeUseAsCStringLen bs $ \(p, len) ->
                           hashPtrWithSalt p (fromIntegral len) salt

instance Hashable BL.ByteString where
    hashWithSalt = BL.foldlChunks hashWithSalt

#if defined(__GLASGOW_HASKELL__)
instance Hashable T.Text where
    hashWithSalt salt (T.Text arr off len) =
        hashByteArrayWithSalt (TA.aBA arr) (off `shiftL` 1) (len `shiftL` 1)
        salt

instance Hashable TL.Text where
    hashWithSalt = TL.foldlChunks hashWithSalt
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
    hashWithSalt = hashUsing hashThreadId
    {-# INLINE hashWithSalt #-}


-- | Compute the hash of a TypeRep, in various GHC versions we can do this quickly.
hashTypeRep :: Int -> TypeRep -> Int
{-# INLINE hashTypeRep #-}
#if __GLASGOW_HASKELL__ >= 702
-- Fingerprint is just the MD5, so taking any Int from it is fine
hashTypeRep salt (TypeRep (Fingerprint x _) _ _) = hashWithSalt salt x
#elif __GLASGOW_HASKELL__ >= 606
hashTypeRep = hashUsing (B.inlinePerformIO . typeRepKey)
#else
hashTypeRep = hashUsing show
#endif

instance Hashable TypeRep where
    hashWithSalt = hashTypeRep
    {-# INLINE hashWithSalt #-}

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
hashByteArrayWithSalt ba !off !len !h =
    fromIntegral $ c_hashByteArray ba (fromIntegral off) (fromIntegral len)
    (fromIntegral h)

foreign import ccall unsafe "hashable_fnv_hash_offset" c_hashByteArray
    :: ByteArray# -> CLong -> CLong -> CLong -> CLong
#endif
