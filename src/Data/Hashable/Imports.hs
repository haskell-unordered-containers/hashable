-- | This module exists to avoid conditional imports
-- and unused import warnings.
{-# LANGUAGE Safe #-}
module Data.Hashable.Imports (
    Int64, Int32,
    Word64, Word32,
    xor, shiftR, shiftL,
) where

import Prelude ()
import Data.Int (Int64, Int32)
import Data.Word (Word64, Word32)
import Data.Bits (xor, shiftR, shiftL)
