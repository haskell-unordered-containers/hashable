{-# LANGUAGE BangPatterns, MagicHash, Rank2Types, UnboxedTuples #-}

-- | Tests for the 'Data.Hashable' module.  We test functions by
-- comparing the C and Haskell implementations.

module Main (main) where

import Data.Hashable (hashByteArray, hashPtr)
import Foreign (unsafePerformIO)
import Foreign.Marshal.Array (withArray)
import GHC.Base (ByteArray#, Int(..), newByteArray#, unsafeCoerce#,
                 writeWord8Array#)
import GHC.ST (ST(..), runST)
import GHC.Word (Word8(..))
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Batch

------------------------------------------------------------------------
-- * Properties

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR  (a,b) g = case randomR (fromIntegral a :: Integer,
                                         fromIntegral b :: Integer) g of
                            (x,g') -> (fromIntegral x, g')

instance Random Word8 where
    randomR = integralRandomR
    random = randomR (minBound,maxBound)

instance Arbitrary Word8 where
    arbitrary = choose (97, 105)
    coarbitrary c = variant (fromIntegral ((fromIntegral c) `rem` 4))

-- | Validate the implementation by comparing the C and Haskell
-- versions.
pHash :: [Word8] -> Bool
pHash xs = unsafePerformIO $ withArray xs $ \ p ->
    (hashByteArray (fromList xs) 0 len ==) `fmap` hashPtr p len
  where len = length xs

tests :: [TestOptions -> IO TestResult]
tests =
    [ run pHash
    ]

-- This wrapper is required by 'runST'.
data ByteArray = BA { unBA :: ByteArray# }

-- | Create a 'ByteArray#' from a list of 'Word8' values.
fromList :: [Word8] -> ByteArray#
fromList xs0 = unBA (runST $ ST $ \ s1# ->
    case newByteArray# len# s1# of
        (# s2#, marr# #) -> case go s2# 0 marr# xs0 of
            s3# -> (# s3#, BA (unsafeCoerce# marr#) #))
  where
    !(I# len#) = length xs0
    go s# _         _     []           = s#
    go s# i@(I# i#) marr# ((W8# x):xs) =
        case writeWord8Array# marr# i# x s# of
            s2# -> go s2# (i + 1) marr# xs

------------------------------------------------------------------------
-- Test harness

options :: TestOptions
options = TestOptions
    { no_of_tests     = 1000
    , length_of_tests = 1
    , debug_tests     = False
    }

main :: IO ()
main = runTests "Bernstein's hash" options tests
