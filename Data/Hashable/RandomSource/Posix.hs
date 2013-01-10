{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Data.Hashable.RandomSource.Posix
    (
      getRandomBytes_
    ) where

import Data.ByteString as B
import Data.ByteString.Internal (create)
import Foreign.C.Error (throwErrnoIfMinus1_)
#if MIN_VERSION_base(4,5,0)
import Foreign.C.Types (CInt(CInt))
#else
import Foreign.C.Types (CInt)
#endif
import Foreign.Ptr (Ptr)

getRandomBytes_ :: String -> Ptr a -> Int -> IO ()
getRandomBytes_ what ptr nbytes = do
  throwErrnoIfMinus1_ what $ c_getRandomBytes ptr (fromIntegral nbytes)

foreign import ccall unsafe "hashable_getRandomBytes" c_getRandomBytes
    :: Ptr a -> CInt -> IO CInt
