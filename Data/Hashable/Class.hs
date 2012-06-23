{-# LANGUAGE Safe #-}

------------------------------------------------------------------------
-- |
-- Module      :  Data.Hashable.Class
-- Copyright   :  (c) Milan Straka 2010
--                (c) Johan Tibell 2011
--                (c) Bryan O'Sullivan 2011
-- License     :  BSD-style
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module defines a class, 'Hashable', for types that can be
-- converted to a hash value.

module Data.Hashable.Class
    (
      Hashable(..), combine, defaultSalt
    ) where

import Data.Bits (xor)

infixl 0 `combine`, `hashWithSalt`

-- | A default salt used in the default implementation of 'hashWithSalt'.
-- It is specified by FNV-1 hash as a default salt for hashing string like
-- types.
defaultSalt :: Int
defaultSalt = 2166136261
{-# INLINE defaultSalt #-}

-- | The class of types that can be converted to a hash value.
--
-- Minimal implementation: 'hash' or 'hashWithSalt'.
class Hashable a where
    -- | Return a hash value for the argument.
    --
    -- The general contract of 'hash' is:
    --
    --  * This integer need not remain consistent from one execution
    --    of an application to another execution of the same
    --    application.
    --
    --  * If two values are equal according to the '==' method, then
    --    applying the 'hash' method on each of the two values must
    --    produce the same integer result.
    --
    --  * It is /not/ required that if two values are unequal
    --    according to the '==' method, then applying the 'hash'
    --    method on each of the two values must produce distinct
    --    integer results.  However, the programmer should be aware
    --    that producing distinct integer results for unequal values
    --    may improve the performance of hashing-based data
    --    structures.
    hash :: a -> Int
    hash = hashWithSalt defaultSalt

    -- | Return a hash value for the argument, using the given salt.
    --
    -- This method can be used to compute different hash values for
    -- the same input by providing a different salt in each
    -- application of the method.
    --
    -- The contract for 'hashWithSalt' is the same as for 'hash', with
    -- the additional requirement that any instance that defines
    -- 'hashWithSalt' must make use of the salt in its implementation.
    hashWithSalt :: Int -> a -> Int
    hashWithSalt salt x = salt `combine` hash x

-- | Combine two given hash values.  'combine' has zero as a left
-- identity.
combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2
