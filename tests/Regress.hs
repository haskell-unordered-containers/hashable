{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Regress (regressions) where
import qualified Test.Framework as F
import Control.Monad (when)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertFailure, (@=?))
import GHC.Generics (Generic)
import Data.List (nub)
import Data.Fixed (Pico)
import Data.Text (Text)
import Data.ByteString (ByteString)

import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8

#ifdef HAVE_MMAP
import qualified Regress.Mmap as Mmap
#endif

import Data.Hashable

#include "MachDeps.h"

regressions :: [F.Test]
regressions = [] ++
#ifdef HAVE_MMAP
    Mmap.regressions ++
    [ testCase "Fixed" $ do
        (hash (1 :: Pico) == hash (2 :: Pico)) @=? False
    ] ++
#endif
    [ F.testGroup "Generic: sum of nullary constructors"
        [ testCase "0" $ nullaryCase 0 S0
        , testCase "1" $ nullaryCase 1 S1
        , testCase "2" $ nullaryCase 2 S2
        , testCase "3" $ nullaryCase 3 S3
        , testCase "4" $ nullaryCase 4 S4
        ]
    , testCase "Generic: Peano https://github.com/tibbe/hashable/issues/135" $ do
        let ns = take 20 $ iterate S Z
        let hs = map hash ns
        hs @=? nub hs
#if WORD_SIZE_IN_BITS == 64
    , testCase "64 bit Text" $ do
          (8403009751224113351) -- siphash
          -- -3875242662334356092 -- FNV
          @=?
          hash ("hello world" :: Text)
#endif
    , F.testGroup "different chars aren't equal"
      [ testCase "String" $ do
            let lhs, rhs :: String
                lhs = "a"
                rhs = "x"

            when (hash lhs == hash rhs) $ do
                assertFailure "Should have different hashes"

        , testCase "Text" $ do
            let lhs, rhs :: Text
                lhs = "a"
                rhs = "x"

            when (hash lhs == hash rhs) $ do
                assertFailure "Should have different hashes"

        , testCase "Lazy Text" $ do
            let lhs, rhs :: TL.Text
                lhs = "a"
                rhs = "x"

            when (hash lhs == hash rhs) $ do
                assertFailure "Should have different hashes"

        , testCase "ByteString" $ do
            let lhs, rhs :: ByteString
                lhs = "a"
                rhs = "x"

            when (hash lhs == hash rhs) $ do
                assertFailure "Should have different hashes"

        , testCase "Lazy ByteString" $ do
            let lhs, rhs :: BSL.ByteString
                lhs = "a"
                rhs = "x"

            when (hash lhs == hash rhs) $ do
                assertFailure "Should have different hashes"

      ]
    , F.testGroup "concatenation"
        [ testCase "String" $ do
            let lhs, rhs :: (String, String)
                lhs = ("foo", "bar")
                rhs = ("foobar", "")

            when (hash lhs == hash rhs) $ do
                assertFailure "Should have different hashes"

        , testCase "Text" $ do
            let lhs, rhs :: (Text, Text)
                lhs = ("foo", "bar")
                rhs = ("foobar", "")

            when (hash lhs == hash rhs) $ do
                assertFailure "Should have different hashes"

        , testCase "Lazy Text" $ do
            let lhs, rhs :: (TL.Text, TL.Text)
                lhs = ("foo", "bar")
                rhs = ("foobar", "")

            when (hash lhs == hash rhs) $ do
                assertFailure "Should have different hashes"

        , testCase "ByteString" $ do
            let lhs, rhs :: (ByteString, ByteString)
                lhs = (BS8.pack "foo", BS8.pack "bar")
                rhs = (BS8.pack "foobar", BS8.empty)

            when (hash lhs == hash rhs) $ do
                assertFailure "Should have different hashes"

        , testCase "Lazy ByteString" $ do
            let lhs, rhs :: (BSL.ByteString, BSL.ByteString)
                lhs = (BSL8.pack "foo", BSL8.pack "bar")
                rhs = (BSL8.pack "foobar", BSL.empty)

            when (hash lhs == hash rhs) $ do
                assertFailure "Should have different hashes"
        ]
    ]
  where
    nullaryCase :: Int -> SumOfNullary -> IO ()
    nullaryCase n s = do
        let salt = 42
        let expected = salt `hashWithSalt` n `hashWithSalt` ()
        let actual = hashWithSalt salt s
        expected @=? actual

data SumOfNullary = S0 | S1 | S2 | S3 | S4 deriving (Generic)
instance Hashable SumOfNullary

data Nat = Z | S Nat deriving (Generic)
instance Hashable Nat
