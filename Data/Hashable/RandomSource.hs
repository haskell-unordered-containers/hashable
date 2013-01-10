{-# LANGUAGE CPP #-}

module Data.Hashable.RandomSource
    (
      getRandomBytes
    , getRandomBytes_
    ) where

import Data.ByteString as B
import Data.ByteString.Internal (create)
#ifdef _WIN32
import Data.Hashable.RandomSource.Windows
#else
import Data.Hashable.RandomSource.Posix
#endif

getRandomBytes :: Int -> IO ByteString
getRandomBytes nbytes
    | nbytes <= 0 = return B.empty
    | otherwise = create nbytes $ flip (getRandomBytes_ "getRandomBytes") nbytes
