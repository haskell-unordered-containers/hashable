{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Data.Hashable.RandomSource
    (
      getRandomBytes
    ) where

import Data.ByteString as B
import Data.ByteString.Internal (create)
import Data.Word (Word8)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)

getRandomBytes :: Int -> IO ByteString
getRandomBytes nbytes
    | nbytes <= 0 = return B.empty
    | otherwise =
  create nbytes $
    throwErrnoIfMinus1_ "getRandomBytes" .
    c_getRandomBytes (fromIntegral nbytes)

foreign import ccall unsafe "hashable_getRandomBytes" c_getRandomBytes
    :: CInt -> Ptr Word8 -> IO CInt
