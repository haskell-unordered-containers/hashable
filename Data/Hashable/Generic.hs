{-# LANGUAGE BangPatterns, FlexibleInstances, KindSignatures,
             ScopedTypeVariables, TypeOperators,
             MultiParamTypeClasses, GADTs, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

------------------------------------------------------------------------
-- |
-- Module      :  Data.Hashable.Generic
-- Copyright   :  (c) Bryan O'Sullivan 2012
-- License     :  BSD-style
-- Maintainer  :  bos@serpentine.com
-- Stability   :  provisional
-- Portability :  GHC >= 7.2
--
-- Hashable support for GHC generics.

module Data.Hashable.Generic
    (
    ) where

import Data.Bits (shiftR)
import Data.Hashable.Class
import GHC.Generics


-- Type without constructors
instance GHashable arity V1 where
    ghashWithSalt _ salt _ = hashWithSalt salt ()

-- Constructor without arguments
instance GHashable arity U1 where
    ghashWithSalt _ salt U1 = hashWithSalt salt ()

instance (GHashable arity a, GHashable arity b) => GHashable arity (a :*: b) where
    ghashWithSalt toHash salt (x :*: y) =
      (ghashWithSalt toHash (ghashWithSalt toHash salt x) y)

-- Metadata (constructor name, etc)
instance GHashable arity a => GHashable arity (M1 i c a) where
    ghashWithSalt targs salt = ghashWithSalt targs salt . unM1

-- Constants, additional parameters, and rank-1 recursion
instance Hashable a => GHashable arity (K1 i a) where
    ghashWithSalt _ = hashUsing unK1

instance GHashable One Par1 where
    ghashWithSalt (HashArgs1 h) salt = h salt . unPar1

instance Hashable1 f => GHashable One (Rec1 f) where
    ghashWithSalt (HashArgs1 h) salt = liftHashWithSalt h salt . unRec1

instance (Hashable1 f, GHashable One g) => GHashable One (f :.: g) where
    ghashWithSalt targs salt = liftHashWithSalt (ghashWithSalt targs) salt . unComp1

class GSum arity f where
    hashSum :: HashArgs arity a -> Int -> Int -> Int -> f a -> Int

instance (GSum arity a, GSum arity b, SumSize a, SumSize b) => GHashable arity (a :+: b) where
    ghashWithSalt toHash salt = hashSum toHash salt 0 size
        where size = unTagged (sumSize :: Tagged (a :+: b))

instance (GSum arity a, GSum arity b) => GSum arity (a :+: b) where
    hashSum toHash !salt !code !size s = case s of
        L1 x -> hashSum toHash salt code           sizeL x
        R1 x -> hashSum toHash salt (code + sizeL) sizeR x
      where
        sizeL = size `shiftR` 1
        sizeR = size - sizeL
    {-# INLINE hashSum #-}

instance GHashable arity a => GSum arity (C1 c a) where
    -- hashSum toHash !salt !code _ (M1 x) = ghashWithSalt toHash (hashWithSalt salt code) x
    hashSum toHash !salt !code _ (M1 x) = hashWithSalt salt (ghashWithSalt toHash code x)
    {-# INLINE hashSum #-}

class SumSize f where
    sumSize :: Tagged f

newtype Tagged (s :: * -> *) = Tagged {unTagged :: Int}

instance (SumSize a, SumSize b) => SumSize (a :+: b) where
    sumSize = Tagged $ unTagged (sumSize :: Tagged a) +
                       unTagged (sumSize :: Tagged b)

instance SumSize (C1 c a) where
    sumSize = Tagged 1

