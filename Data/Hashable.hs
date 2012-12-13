{-# LANGUAGE CPP #-}

------------------------------------------------------------------------
-- |
-- Module      :  Data.Hashable
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
-- most standard types.  Efficient instances for other types can be
-- generated automatically and effortlessly using the generics support
-- in GHC 7.2 and above.
--
-- The easiest way to get started is to use the 'hash' function. Here
-- is an example session with @ghci@.
--
-- > Prelude> import Data.Hashable
-- > Prelude> hash "foo"
-- > 60853164

module Data.Hashable
    (
      -- * Computing hash values
      hash
    , Hashable(..)

      -- ** Avalanche behavior
      -- $avalanche

      -- * Creating new instances
      -- | There are two ways to create new instances: by deriving
      -- instances automatically using GHC's generic programming
      -- support or by writing instances manually.

      -- ** Generic instances
      -- $generics

      -- *** Understanding a compiler error
      -- $generic_err

      -- ** Writing instances by hand
      -- $blocks
    , hashUsing
    , hashPtr
    , hashPtrWithSalt
#if defined(__GLASGOW_HASKELL__)
    , hashByteArray
    , hashByteArrayWithSalt
#endif
      -- ** Hashing types with multiple constructors
      -- $ctors
    ) where

import Data.Hashable.Class
#ifdef GENERICS
import Data.Hashable.Generic ()
#endif

-- $avalanche
--
-- A good hash function has a 50% probability of flipping every bit of
-- its result in response to a change of just one bit in its
-- input. This property is called /avalanche/. To be truly general
-- purpose, hash functions must have strong avalanche behavior.
--
-- All of the 'Hashable' instances provided by this module have
-- excellent avalanche properties.

-- $generics
--
-- Beginning with GHC 7.2, the recommended way to make instances of
-- 'Hashable' for most types is to use the compiler's support for
-- automatically generating default instances.
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > import GHC.Generics (Generic)
-- > import Data.Hashable
-- >
-- > data Foo a = Foo a String
-- >              deriving (Eq, Generic)
-- >
-- > instance Hashable a => Hashable (Foo a)
-- >
-- > data Colour = Red | Green | Blue
-- >               deriving Generic
-- >
-- > instance Hashable Colour
--
-- If you omit a body for the instance declaration, GHC will generate
-- a default instance that correctly and efficiently hashes every
-- constructor and parameter.

-- $generic_err
--
-- Suppose you intend to use the generic machinery to automatically
-- generate a 'Hashable' instance.
--
-- > data Oops = Oops
-- >      -- forgot to add "deriving Generic" here!
-- >
-- > instance Hashable Oops
--
-- And imagine that, as in the example above, you forget to add a
-- \"@deriving 'Generic'@\" clause to your data type. At compile time,
-- you will get an error message from GHC that begins roughly as
-- follows:
--
-- > No instance for (GHashable (Rep Oops))
--
-- This error can be confusing, as 'GHashable' is not exported (it is
-- an internal typeclass used by this library's generics machinery).
-- The correct fix is simply to add the missing \"@deriving
-- 'Generic'@\".

-- $blocks
--
-- To maintain high quality hashes, new 'Hashable' instances should be
-- built using existing 'Hashable' instances, combinators, and hash
-- functions.
--
-- The functions below can be used when creating new instances of
-- 'Hashable'.  For many string-like types the
-- 'hashWithSalt' method can be defined in terms of either
-- 'hashPtrWithSalt' or 'hashByteArrayWithSalt'.  Here's how you could
-- implement an instance for the 'B.ByteString' data type, from the
-- @bytestring@ package:
--
-- > import qualified Data.ByteString as B
-- > import qualified Data.ByteString.Internal as B
-- > import qualified Data.ByteString.Unsafe as B
-- > import Data.Hashable
-- > import Foreign.Ptr (castPtr)
-- >
-- > instance Hashable B.ByteString where
-- >     hashWithSalt salt bs = B.inlinePerformIO $
-- >                            B.unsafeUseAsCStringLen bs $ \(p, len) ->
-- >                            hashPtrWithSalt p (fromIntegral len) salt
--
-- Use 'hashWithSalt' to compute a hash from several values, using
-- this recipe:
--
-- > data Product a b = P a b
-- >
-- > instance (Hashable a, Hashable b) => Hashable (Product a b) where
-- >     hashWithSalt s (P a b) = s `hashWithSalt` a `hashWithSalt` b
--
-- You can chain hashes together using 'hashWithSalt', by following
-- this recipe:
--
-- > combineTwo h1 h2 = h1 `hashWithSalt` h2

-- $ctors
--
-- For a type with several value constructors, there are a few
-- possible approaches to writing a 'Hashable' instance.
--
-- If the type is an instance of 'Enum', the easiest (and safest) path
-- is to convert it to an 'Int', and use the existing 'Hashable'
-- instance for 'Int'.
--
-- > data Color = Red | Green | Blue
-- >              deriving Enum
-- >
-- > instance Hashable Color where
-- >     hashWithSalt = hashUsing fromEnum
--
-- This instance benefits from the fact that the 'Hashable' instance
-- for 'Int' has excellent avalanche properties.
--
-- In contrast, a very weak hash function would be:
--
-- > terribleHash :: Color -> Int
-- > terribleHash salt = fromEnum
--
-- This has terrible avalanche properties, as the salt is ignored, and
-- every input is mapped to a small integer.
--
-- If the type's constructors accept parameters, it can be important
-- to distinguish the constructors.
--
-- > data Time = Days Int
-- >           | Weeks Int
-- >           | Months Int
--
-- The weak hash function below guarantees a high probability of days,
-- weeks, and months all colliding when hashed.
--
-- > veryBadHash :: Time -> Int
-- > veryBadHash (Days  d)  = hash d
-- > veryBadHash (Weeks w)  = hash w
-- > veryBadHash (Months m) = hash m
--
-- It is easy to distinguish the constructors using the `hashWithSalt`
-- function.
--
-- > instance Hashable Time where
-- >     hashWithSalt s (Days n)   = s `hashWithSalt`
-- >                                 (0::Int) `hashWithSalt` n
-- >     hashWithSalt s (Weeks n)  = s `hashWithSalt`
-- >                                 (1::Int) `hashWithSalt` n
-- >     hashWithSalt s (Months n) = s `hashWithSalt`
-- >                                 (2::Int) `hashWithSalt` n
--
-- If a constructor accepts multiple parameters, their hashes can be
-- chained.
--
-- > data Date = Date Int Int Int
-- >
-- > instance Hashable Date where
-- >     hashWithSalt s (Date yr mo dy) =
-- >         s `hashWithSalt`
-- >         yr `hashWithSalt`
-- >         mo `hashWithSalt` dy
