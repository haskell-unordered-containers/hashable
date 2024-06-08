-- | This module exists to avoid conditional imports
-- and unused import warnings.
{-# LANGUAGE Safe #-}
module Data.Hashable.Imports (
    Int64, Int32,
    Word64, Word32,
    xor, shiftR, shiftL, unsafeShiftL, unsafeShiftR,
    (.&.),
) where

import Data.Bits (shiftL, shiftR, unsafeShiftL, unsafeShiftR, xor, (.&.))
import Data.Int  (Int32, Int64)
import Data.Word (Word32, Word64)
import Prelude ()
