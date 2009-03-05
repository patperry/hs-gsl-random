----------------------------------------------------------------------------
-- |
-- Module     : GSL.Random.Quasi
-- Copyright  : Copyright (c) 2009 , Tracy Wadleigh
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--
-- Quasi-random number generators.

module GSL.Random.Quasi (
    module GSL.Random.Quasi.Internal
    ) where

import GSL.Random.Quasi.Internal hiding ( MkQRNG )