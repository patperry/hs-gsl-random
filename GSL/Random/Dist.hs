{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module     : GSL.Random.Dist
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module GSL.Random.Dist (
    getPoisson,
    getFlat,
    getGaussian,
    ) where

import Foreign.C.Types      ( CUInt, CDouble )
import Foreign.ForeignPtr   ( withForeignPtr )
import Foreign.Ptr          ( Ptr )

import GSL.Random.Gen.Internal ( RNG(..) )

-- | @getPoisson r mu@ gets a poisson random variable with mean @mu@.
getPoisson :: RNG -> Double -> IO Int
getPoisson (MkRNG fptr) mu =
    let mu' = realToFrac mu
    in withForeignPtr fptr $ \ptr -> do
        x <- gsl_ran_poisson ptr mu' 
        return $ (fromInteger . toInteger) x
                    
foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_poisson :: Ptr () -> CDouble -> IO CUInt

-- | @getFlat r a b@ gets a value uniformly chosen in @[a,b)@.
getFlat :: RNG -> Double -> Double -> IO (Double)
getFlat (MkRNG fptr) a b  =
    let a' = realToFrac a
        b' = realToFrac b
    in withForeignPtr fptr $ \ptr -> do
            x <- gsl_ran_flat ptr a' b'
            return $ realToFrac x
        
foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_flat :: Ptr () -> CDouble -> CDouble -> IO CDouble


-- | @getGaussian r sigma@ gets a normal random variable with mean
-- @0@ and standard deviation @sigma@.
getGaussian :: RNG -> Double -> IO Double
getGaussian (MkRNG fptr) sigma  =
    let sigma' = realToFrac sigma
    in withForeignPtr fptr $ \ptr -> do
        x <- gsl_ran_gaussian ptr sigma'
        return $ realToFrac x
        
foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_gaussian :: Ptr () -> CDouble -> IO CDouble
    