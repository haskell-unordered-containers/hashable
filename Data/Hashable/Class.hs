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
      Hashable(..)
#ifdef GENERICS
      -- ** Support for generics
    , GHashable(..)
#endif

      -- * Creating new instances
    , hashUsing
    , hashPtr
    , hashPtrWithSalt
    , hashByteArray
    , hashByteArrayWithSalt
    ) where

import Control.Exception (assert)
import Data.Bits (shiftL, shiftR, xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as B
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List (foldl')
import Data.Ratio (Ratio, denominator, numerator)
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import qualified Data.Text.Internal as T
import qualified Data.Text.Lazy as TL
import Data.Typeable
import Data.Version (Version(..))
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C (CString)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (alignment, peek, sizeOf)
import GHC.Base (ByteArray#)
import GHC.Conc (ThreadId(..))
import GHC.Prim (ThreadId#)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName
import Data.Unique (Unique, hashUnique)

#if MIN_VERSION_base(4,7,0)
import Data.Fixed (Fixed(..))
#endif

#ifdef GENERICS
import GHC.Generics
#endif

#if __GLASGOW_HASKELL__ >= 710
import GHC.Fingerprint.Type(Fingerprint(..))
#elif __GLASGOW_HASKELL__ >= 702
import Data.Typeable.Internal(TypeRep(..))
import GHC.Fingerprint.Type(Fingerprint(..))
#endif

#if __GLASGOW_HASKELL__ >= 703
import Foreign.C (CLong(..))
import Foreign.C.Types (CInt(..))
#else
import Foreign.C (CLong)
import Foreign.C.Types (CInt)
#endif

#if !(MIN_VERSION_base(4,8,0))
import Data.Word (Word)
#endif

#if MIN_VERSION_base(4,7,0)
import Data.Bits (finiteBitSize)
#else
import Data.Bits (bitSize)
#endif

#if !(MIN_VERSION_bytestring(0,10,0))
import qualified Data.ByteString.Lazy.Internal as BL  -- foldlChunks
#endif

#if MIN_VERSION_bytestring(0,10,4)
import qualified Data.ByteString.Short.Internal as BSI
#endif

#ifdef VERSION_integer_gmp

# if MIN_VERSION_integer_gmp(1,0,0)
#  define MIN_VERSION_integer_gmp_1_0_0
# endif

import GHC.Exts (Int(..))
import GHC.Integer.GMP.Internals (Integer(..))
# if defined(MIN_VERSION_integer_gmp_1_0_0)
import GHC.Exts (sizeofByteArray#)
import GHC.Integer.GMP.Internals (BigNat(BN#))
# endif
#endif

#if MIN_VERSION_base(4,8,0)
import Data.Void (Void, absurd)
import GHC.Natural (Natural(..))
import GHC.Exts (Word(..))
#endif

#if MIN_VERSION_base(4,9,0)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup
#endif

#include "MachDeps.h"

infixl 0 `hashWithSalt`

------------------------------------------------------------------------
-- * Computing hash values

-- | A default salt used in the implementation of 'hash'.
defaultSalt :: Int
#if WORD_SIZE_IN_BITS == 64
defaultSalt = -2578643520546668380  -- 0xdc36d1615b7400a4
#else
defaultSalt = 0x087fc72c
#endif
{-# INLINE defaultSalt #-}

-- | The class of types that can be converted to a hash value.
--
-- Minimal implementation: 'hashWithSalt'.
class Hashable a where
    -- | Return a hash value for the argument, using the given salt.
    --
    -- The general contract of 'hashWithSalt' is:
    --
    --  * If two values are equal according to the '==' method, then
    --    applying the 'hashWithSalt' method on each of the two values
    --    /must/ produce the same integer result if the same salt is
    --    used in each case.
    --
    --  * It is /not/ required that if two values are unequal
    --    according to the '==' method, then applying the
    --    'hashWithSalt' method on each of the two values must produce
    --    distinct integer results. However, the programmer should be
    --    aware that producing distinct integer results for unequal
    --    values may improve the performance of hashing-based data
    --    structures.
    --
    --  * This method can be used to compute different hash values for
    --    the same input by providing a different salt in each
    --    application of the method. This implies that any instance
    --    that defines 'hashWithSalt' /must/ make use of the salt in
    --    its implementation.
    hashWithSalt :: Int -> a -> Int

    -- | Like 'hashWithSalt', but no salt is used. The default
    -- implementation uses 'hashWithSalt' with some default salt.
    -- Instances might want to implement this method to provide a more
    -- efficient implementation than the default implementation.
    hash :: a -> Int
    hash = hashWithSalt defaultSalt

#ifdef GENERICS
    default hashWithSalt :: (Generic a, GHashable (Rep a)) => Int -> a -> Int
    hashWithSalt salt = ghashWithSalt salt . from

-- | The class of types that can be generically hashed.
class GHashable f where
    ghashWithSalt :: Int -> f a -> Int
#endif

-- Since we support a generic implementation of 'hashWithSalt' we
-- cannot also provide a default implementation for that method for
-- the non-generic instance use case. Instead we provide
-- 'defaultHashWith'.

defaultHashWithSalt :: Hashable a => Int -> a -> Int
defaultHashWithSalt salt x = salt `combine` hash x

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

instance Hashable Int where
    hash = id
    hashWithSalt = defaultHashWithSalt

instance Hashable Int8 where
    hash = fromIntegral
    hashWithSalt = defaultHashWithSalt

instance Hashable Int16 where
    hash = fromIntegral
    hashWithSalt = defaultHashWithSalt

instance Hashable Int32 where
    hash = fromIntegral
    hashWithSalt = defaultHashWithSalt

instance Hashable Int64 where
    hash n
#if MIN_VERSION_base(4,7,0)
        | finiteBitSize (undefined :: Int) == 64 = fromIntegral n
#else
        | bitSize (undefined :: Int) == 64 = fromIntegral n
#endif
        | otherwise = fromIntegral (fromIntegral n `xor`
                                   (fromIntegral n `shiftR` 32 :: Word64))
    hashWithSalt = defaultHashWithSalt

instance Hashable Word where
    hash = fromIntegral
    hashWithSalt = defaultHashWithSalt

instance Hashable Word8 where
    hash = fromIntegral
    hashWithSalt = defaultHashWithSalt

instance Hashable Word16 where
    hash = fromIntegral
    hashWithSalt = defaultHashWithSalt

instance Hashable Word32 where
    hash = fromIntegral
    hashWithSalt = defaultHashWithSalt

instance Hashable Word64 where
    hash n
#if MIN_VERSION_base(4,7,0)
        | finiteBitSize (undefined :: Int) == 64 = fromIntegral n
#else
        | bitSize (undefined :: Int) == 64 = fromIntegral n
#endif
        | otherwise = fromIntegral (n `xor` (n `shiftR` 32))
    hashWithSalt = defaultHashWithSalt

instance Hashable () where
    hash = fromEnum
    hashWithSalt = defaultHashWithSalt

instance Hashable Bool where
    hash = fromEnum
    hashWithSalt = defaultHashWithSalt

instance Hashable Ordering where
    hash = fromEnum
    hashWithSalt = defaultHashWithSalt

instance Hashable Char where
    hash = fromEnum
    hashWithSalt = defaultHashWithSalt

#if defined(MIN_VERSION_integer_gmp_1_0_0)
instance Hashable BigNat where
    hashWithSalt salt (BN# ba) = hashByteArrayWithSalt ba 0 numBytes salt
                                 `hashWithSalt` size
      where
        size     = numBytes `quot` SIZEOF_HSWORD
        numBytes = I# (sizeofByteArray# ba)
#endif

#if MIN_VERSION_base(4,8,0)
instance Hashable Natural where
# if defined(MIN_VERSION_integer_gmp_1_0_0)
    hash (NatS# n)   = hash (W# n)
    hash (NatJ# bn)  = hash bn

    hashWithSalt salt (NatS# n)   = hashWithSalt salt (W# n)
    hashWithSalt salt (NatJ# bn)  = hashWithSalt salt bn
# else
    hash (Natural n) = hash n

    hashWithSalt salt (Natural n) = hashWithSalt salt n
# endif
#endif

instance Hashable Integer where
#if defined(VERSION_integer_gmp)
# if defined(MIN_VERSION_integer_gmp_1_0_0)
    hash (S# n)   = (I# n)
    hash (Jp# bn) = hash bn
    hash (Jn# bn) = negate (hash bn)

    hashWithSalt salt (S# n)   = hashWithSalt salt (I# n)
    hashWithSalt salt (Jp# bn) = hashWithSalt salt bn
    hashWithSalt salt (Jn# bn) = negate (hashWithSalt salt bn)
# else
    hash (S# int) = I# int
    hash n@(J# size# byteArray)
        | n >= minInt && n <= maxInt = fromInteger n :: Int
        | otherwise = let size = I# size#
                          numBytes = SIZEOF_HSWORD * abs size
                      in hashByteArrayWithSalt byteArray 0 numBytes defaultSalt
                         `hashWithSalt` size
      where minInt = fromIntegral (minBound :: Int)
            maxInt = fromIntegral (maxBound :: Int)

    hashWithSalt salt (S# n) = hashWithSalt salt (I# n)
    hashWithSalt salt n@(J# size# byteArray)
        | n >= minInt && n <= maxInt = hashWithSalt salt (fromInteger n :: Int)
        | otherwise = let size = I# size#
                          numBytes = SIZEOF_HSWORD * abs size
                      in hashByteArrayWithSalt byteArray 0 numBytes salt
                         `hashWithSalt` size
      where minInt = fromIntegral (minBound :: Int)
            maxInt = fromIntegral (maxBound :: Int)
# endif
#else
    hashWithSalt salt = foldl' hashWithSalt salt . go
      where
        go n | inBounds n = [fromIntegral n :: Int]
             | otherwise   = fromIntegral n : go (n `shiftR` WORD_SIZE_IN_BITS)
        maxInt = fromIntegral (maxBound :: Int)
        inBounds x = x >= fromIntegral (minBound :: Int) && x <= maxInt
#endif

#if MIN_VERSION_base(4,9,0)
-- Starting with base-4.9, numerator/denominator don't need 'Integral' anymore
instance Hashable a => Hashable (Ratio a) where
#else
instance (Integral a, Hashable a) => Hashable (Ratio a) where
#endif
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
    hashWithSalt = defaultHashWithSalt

instance Hashable Double where
    hash x
        | isIEEE x =
            assert (sizeOf x >= sizeOf (0::Word64) &&
                    alignment x >= alignment (0::Word64)) $
            hash ((unsafePerformIO $ with x $ peek . castPtr) :: Word64)
        | otherwise = hash (show x)
    hashWithSalt = defaultHashWithSalt

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

instance Hashable (StableName a) where
    hash = hashStableName
    hashWithSalt = defaultHashWithSalt

-- Auxillary type for Hashable [a] definition
data SPInt = SP !Int !Int

instance Hashable a => Hashable [a] where
    {-# SPECIALIZE instance Hashable [Char] #-}
    hashWithSalt salt arr = finalise (foldl' step (SP salt 0) arr)
      where
        finalise (SP s l) = hashWithSalt s l
        step (SP s l) x   = SP (hashWithSalt s x) (l + 1)

instance Hashable B.ByteString where
    hashWithSalt salt bs = B.inlinePerformIO $
                           B.unsafeUseAsCStringLen bs $ \(p, len) ->
                           hashPtrWithSalt p (fromIntegral len) salt

instance Hashable BL.ByteString where
    hashWithSalt = BL.foldlChunks hashWithSalt

#if MIN_VERSION_bytestring(0,10,4)
instance Hashable BSI.ShortByteString where
#if MIN_VERSION_base(4,3,0)
    hashWithSalt salt sbs@(BSI.SBS ba) =
#else
    hashWithSalt salt sbs@(BSI.SBS ba _) =
#endif
        hashByteArrayWithSalt ba 0 (BSI.length sbs) salt
#endif

instance Hashable T.Text where
    hashWithSalt salt (T.Text arr off len) =
        hashByteArrayWithSalt (TA.aBA arr) (off `shiftL` 1) (len `shiftL` 1)
        salt

instance Hashable TL.Text where
    hashWithSalt = TL.foldlChunks hashWithSalt

-- | Compute the hash of a ThreadId.
hashThreadId :: ThreadId -> Int
hashThreadId (ThreadId t) = hash (fromIntegral (getThreadId t) :: Int)

foreign import ccall unsafe "rts_getThreadId" getThreadId
    :: ThreadId# -> CInt

instance Hashable ThreadId where
    hash = hashThreadId
    hashWithSalt = defaultHashWithSalt

-- | Compute the hash of a TypeRep, in various GHC versions we can do this quickly.
hashTypeRep :: TypeRep -> Int
{-# INLINE hashTypeRep #-}
#if __GLASGOW_HASKELL__ >= 710
-- Fingerprint is just the MD5, so taking any Int from it is fine
hashTypeRep tr = let Fingerprint x _ = typeRepFingerprint tr in fromIntegral x
#elif __GLASGOW_HASKELL__ >= 702
-- Fingerprint is just the MD5, so taking any Int from it is fine
hashTypeRep (TypeRep (Fingerprint x _) _ _) = fromIntegral x
#elif __GLASGOW_HASKELL__ >= 606
hashTypeRep = B.inlinePerformIO . typeRepKey
#else
hashTypeRep = hash . show
#endif

instance Hashable TypeRep where
    hash = hashTypeRep
    hashWithSalt = defaultHashWithSalt
    {-# INLINE hash #-}

#if MIN_VERSION_base(4,8,0)
instance Hashable Void where
    hashWithSalt _ = absurd
#endif

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

-- | Compute a hash value for the content of this 'ByteArray#',
-- beginning at the specified offset, using specified number of bytes.
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

-- | Combine two given hash values.  'combine' has zero as a left
-- identity.
combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2

instance Hashable Unique where
    hash = hashUnique
    hashWithSalt = defaultHashWithSalt

instance Hashable Version where
    hashWithSalt salt (Version branch tags) =
        salt `hashWithSalt` branch `hashWithSalt` tags

#if MIN_VERSION_base(4,7,0)
instance Hashable (Fixed a) where
    hashWithSalt salt (MkFixed i) = hashWithSalt salt i
#endif

-- instances formerly provided by 'semigroups' package
#if MIN_VERSION_base(4,9,0)
instance Hashable a => Hashable (NE.NonEmpty a) where
    hashWithSalt p (a NE.:| as) = p `hashWithSalt` a `hashWithSalt` as

instance Hashable a => Hashable (Min a) where
    hashWithSalt p (Min a) = hashWithSalt p a

instance Hashable a => Hashable (Max a) where
    hashWithSalt p (Max a) = hashWithSalt p a

instance (Hashable a, Hashable b) => Hashable (Arg a b) where
    hashWithSalt p (Arg a b) = hashWithSalt p a `hashWithSalt` b

instance Hashable a => Hashable (First a) where
    hashWithSalt p (First a) = hashWithSalt p a

instance Hashable a => Hashable (Last a) where
    hashWithSalt p (Last a) = hashWithSalt p a

instance Hashable a => Hashable (WrappedMonoid a) where
    hashWithSalt p (WrapMonoid a) = hashWithSalt p a

instance Hashable a => Hashable (Option a) where
    hashWithSalt p (Option a) = hashWithSalt p a
#endif
