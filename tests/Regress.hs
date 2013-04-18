{-# LANGUAGE CPP #-}

module Regress (regressions) where

import qualified Test.Framework as F

#ifdef HAVE_MMAP
import qualified Regress.Mmap as Mmap
#endif

regressions :: [F.Test]
regressions = []
#ifdef HAVE_MMAP
              ++ Mmap.regressions
#endif
