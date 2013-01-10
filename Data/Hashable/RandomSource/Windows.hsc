{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Data.Hashable.RandomSource.Windows
    (
      getRandomBytes_
    ) where

#if __GLASGOW_HASKELL__ >= 704
import Foreign.C.Types (CUIntPtr(..))
#else
import Foreign.C.Types (CUIntPtr)
#endif
import Control.Exception (bracket)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import System.Win32.Types (BYTE, DWORD, LPCTSTR, failIfFalse_)

type HCRYPTPROV = CUIntPtr

#include <windows.h>
#include <wincrypt.h>

getRandomBytes_ :: String -> Ptr a -> Int -> IO ()
getRandomBytes_ what ptr nbytes = alloca $ \provPtr -> do
  failIfFalse_ what $ 
    c_cryptAcquireContext provPtr nullPtr nullPtr (#const PROV_RSA_FULL)
      (#const CRYPT_VERIFYCONTEXT)
  bracket (peek provPtr) (\p -> failIfFalse_ what $ 
                                  c_cryptReleaseContext p 0) $ \prov ->
    failIfFalse_ what $
      c_cryptGenRandom prov (fromIntegral nbytes) (castPtr ptr)

foreign import stdcall unsafe "wincrypt.h CryptAcquireContextW" 
    c_cryptAcquireContext :: Ptr HCRYPTPROV -> LPCTSTR -> LPCTSTR -> DWORD 
                          -> DWORD -> IO Bool

foreign import stdcall unsafe "wincrypt.h CryptReleaseContext"
    c_cryptReleaseContext :: HCRYPTPROV -> DWORD -> IO Bool

foreign import stdcall unsafe "wincrypt.h CryptGenRandom"
    c_cryptGenRandom :: HCRYPTPROV -> DWORD -> Ptr BYTE -> IO Bool
