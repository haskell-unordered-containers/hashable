{-# LANGUAGE ForeignFunctionInterface #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Hash
-- Copyright   :  (c) Milan Straka 2010
-- License     :  BSD-style
-- Maintainer  :  fox@ucw.cz
-- Stability   :  provisional
-- Portability :  portable
--
-- 'Hashable' class for hashable types, with instances for basic types. The only
-- function of this class is
--
-- @
--   'hash' :: Hashable h => h -> Int
-- @
--
-- The 'hash' function should be as collision-free as possible, the probability
-- of @'hash' a == 'hash' b@ should ideally be 1 over the number of representable
-- values in an 'Int'.
--
-- Returning an 'Int' is a result of the 'Data.IntMap.IntMap' using 'Int' as
-- a key, as inserting the hash values to the 'Data.IntMap.IntMap' was the
-- purpose of creating this class.
-----------------------------------------------------------------------------

module Data.Hashable ( Hashable(..)
                     , combine
                     ) where

import Data.Bits
import Data.Int
import Data.Word
import Data.List (foldl')
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BInt
import qualified Data.ByteString.Unsafe as BInt
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BLInt
import Foreign.C

-- | The class containing a function 'hash' which computes the hash values of
-- given value.
class Hashable a where
    -- | The computed 'hash' value should be as collision-free as possible, the
    -- probability of @'hash' a == 'hash' b@ should ideally be 1 over the
    -- number of representable values in an 'Int'.
    hash :: a -> Int

-- | Combines two given hash values.
combine :: Int -> Int -> Int
combine h1 h2 = (h1 + h1 `shiftL` 5) `xor` h2

hashAndCombine :: Hashable h => Int -> h -> Int
hashAndCombine acc h = acc `combine` hash h

instance Hashable () where hash _ = 0

instance Hashable Bool where hash x = case x of { True -> 1; False -> 0 }

instance Hashable Int where hash = id
instance Hashable Int8 where hash = fromIntegral
instance Hashable Int16 where hash = fromIntegral
instance Hashable Int32 where hash = fromIntegral
instance Hashable Int64 where hash = fromIntegral

instance Hashable Word where hash = fromIntegral
instance Hashable Word8 where hash = fromIntegral
instance Hashable Word16 where hash = fromIntegral
instance Hashable Word32 where hash = fromIntegral
instance Hashable Word64 where hash = fromIntegral

instance Hashable Char where hash = fromEnum

instance Hashable a => Hashable (Maybe a) where
    hash Nothing = 0
    hash (Just a) = 42 `combine` hash a

instance (Hashable a1, Hashable a2) => Hashable (a1, a2) where
    hash (a1, a2) = hash a1 `combine` hash a2

instance (Hashable a1, Hashable a2, Hashable a3) => Hashable (a1, a2, a3) where
    hash (a1, a2, a3) = hash a1 `combine` hash a2 `combine` hash a3

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4) => Hashable (a1, a2, a3, a4) where
    hash (a1, a2, a3, a4) = hash a1 `combine` hash a2 `combine` hash a3 `combine` hash a4

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5)
      => Hashable (a1, a2, a3, a4, a5) where
    hash (a1, a2, a3, a4, a5) =
      hash a1 `combine` hash a2 `combine` hash a3 `combine` hash a4 `combine` hash a5

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5, Hashable a6)
      => Hashable (a1, a2, a3, a4, a5, a6) where
    hash (a1, a2, a3, a4, a5, a6) =
      hash a1 `combine` hash a2 `combine` hash a3 `combine` hash a4 `combine` hash a5 `combine` hash a6

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5, Hashable a6, Hashable a7)
      => Hashable (a1, a2, a3, a4, a5, a6, a7) where
    hash (a1, a2, a3, a4, a5, a6, a7) =
      hash a1 `combine` hash a2 `combine` hash a3 `combine` hash a4 `combine` hash a5 `combine` hash a6 `combine` hash a7

instance Hashable a => Hashable [a] where
    {-# SPECIALIZE instance Hashable [Char] #-}
    hash = foldl' hashAndCombine 0

foreign import ccall unsafe hashByteString :: CString -> CInt -> IO CInt
instance Hashable B.ByteString where
    hash bstr = fromIntegral $ BInt.inlinePerformIO $ BInt.unsafeUseAsCStringLen bstr $
                  \(str, len) -> hashByteString str (fromIntegral len)

instance Hashable BL.ByteString where hash = BLInt.foldlChunks hashAndCombine 0
