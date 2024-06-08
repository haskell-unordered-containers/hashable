{-# LANGUAGE NumericUnderscores #-}
module Main (main) where

import           Control.Monad.ST (runST)
import qualified Data.ByteString as BS
import qualified Data.Primitive as P
import           Data.Word (Word32, Word64)
import           Test.Tasty (defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase, (@=?))
import           Test.Tasty.QuickCheck (testProperty, (===))

import           Data.Hashable.XXH3

main :: IO ()
main = defaultMain $ testGroup "xxhash"
    [ testGroup "oneshot"
        [ testProperty "w64-ref" $ \w salt ->
            xxh3_64bit_withSeed_w64 w salt === xxh3_64bit_withSeed_w64_ref w salt
        , testCase "w64-examples" $ do
            xxh3_64bit_withSeed_w64 0                     0 @=? 0xc77b_3abb_6f87_acd9
            xxh3_64bit_withSeed_w64 0x12                  1 @=? 0xbba4_8522_c425_46b2
            xxh3_64bit_withSeed_w64 0x2100_0000_0000_0000 0 @=? 0xb7cb_e42a_e127_8055
            xxh3_64bit_withSeed_w64 0x1eb6e9              0 @=? 0x8e_adc3_1b56

        , testProperty "w32-ref" $ \w salt ->
            xxh3_64bit_withSeed_w32 w salt === xxh3_64bit_withSeed_w32_ref w salt

        , testCase "w32-examples" $ do
            xxh3_64bit_withSeed_w32 0                     0 @=? 0x48b2_c926_16fc_193d
            xxh3_64bit_withSeed_w32 0x12                  1 @=? 0x2870_1df3_2a21_6ad3

        ]

    , testGroup "incremental"
        [ testProperty "empty" $ \seed -> do
              let expected = xxh3_64bit_withSeed_bs BS.empty seed
              let actual = runST $ do
                    s <- xxh3_64bit_createState
                    xxh3_64bit_reset_withSeed s seed
                    xxh3_64bit_digest s

              actual === expected

        , testProperty "bs" $ \w8s seed -> do
              let bs = BS.pack w8s
              let expected = xxh3_64bit_withSeed_bs bs seed
              let actual = runST $ do
                    s <- xxh3_64bit_createState
                    xxh3_64bit_reset_withSeed s seed
                    xxh3_64bit_update_bs s bs
                    xxh3_64bit_digest s

              actual === expected
        ]
    ]

xxh3_64bit_withSeed_w64_ref :: Word64 -> Word64 -> Word64
xxh3_64bit_withSeed_w64_ref w salt = case P.primArrayFromList [w] of
        P.PrimArray ba -> xxh3_64bit_withSeed_ba (P.ByteArray ba) 0 8 salt

xxh3_64bit_withSeed_w32_ref :: Word32 -> Word64 -> Word64
xxh3_64bit_withSeed_w32_ref w salt = case P.primArrayFromList [w] of
        P.PrimArray ba -> xxh3_64bit_withSeed_ba (P.ByteArray ba) 0 4 salt
