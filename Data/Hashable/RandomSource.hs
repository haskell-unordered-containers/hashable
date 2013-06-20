{-# LANGUAGE CPP, ForeignFunctionInterface #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

module Data.Hashable.RandomSource
    (
      getRandomBytes
    , getRandomBytes_
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

getRandomBytes :: Int -> IO ByteString
getRandomBytes nbytes
    | nbytes <= 0 = return B.empty
    | otherwise = create nbytes $ flip (getRandomBytes_ "getRandomBytes") nbytes

getRandomBytes_ :: String -> Ptr a -> Int -> IO ()
getRandomBytes_ what ptr nbytes = do
  throwErrnoIfMinus1_ what $ c_getRandomBytes ptr (fromIntegral nbytes)

foreign import ccall unsafe "hashable_getRandomBytes" c_getRandomBytes
    :: Ptr a -> CInt -> IO CInt
