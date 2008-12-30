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
    -- * The Gaussian Distribution
    -- ** General
    gaussianPdf,

    gaussianP,
    gaussianQ,
    gaussianPInv,
    gaussianQInv,

    getGaussian,
    getGaussianZiggurat,
    getGaussianRatioMethod,

    -- ** Unit Variance
    ugaussianPdf,

    ugaussianP,
    ugaussianQ,
    ugaussianPInv,
    ugaussianQInv,

    getUGaussian,
    getUGaussianZiggurat,
    getUGaussianRatioMethod,
    
    -- * The Flat (Uniform) Distribution
    flatPdf,

    flatP,
    flatQ,
    flatPInv,
    flatQInv,
    
    getFlat,
    
    -- * The Poisson Distribution
    poissonPdf,

    poissonP,
    poissonQ,

    getPoisson,
    
    ) where

import Foreign.C.Types      ( CUInt, CDouble )
import Foreign.ForeignPtr   ( withForeignPtr )
import Foreign.Ptr          ( Ptr )

import GSL.Random.Gen.Internal ( RNG(..) )

-- | @gaussianPdf x sigma@ computes the probabililty density p(x) for 
-- a Gaussian distribution with mean @0@ and standard deviation @sigma@.
gaussianPdf :: Double -> Double -> Double
gaussianPdf = liftDouble2 gsl_ran_gaussian_pdf

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_gaussian_pdf :: CDouble -> CDouble -> CDouble

-- | @gaussianP x sigma@ computes the cumulative distribution function P(x) for 
-- a Gaussian distribution with mean @0@ and standard deviation @sigma@.
gaussianP :: Double -> Double -> Double
gaussianP = liftDouble2 gsl_cdf_gaussian_P

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_gaussian_P :: CDouble -> CDouble -> CDouble

-- | @gaussianQ x sigma@ computes the cumulative distribution function Q(x) for 
-- a Gaussian distribution with mean @0@ and standard deviation @sigma@.
gaussianQ :: Double -> Double -> Double
gaussianQ = liftDouble2 gsl_cdf_gaussian_Q

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_gaussian_Q :: CDouble -> CDouble -> CDouble

-- | @gaussianPInv p sigma@ computes the inverse of the cumulative distribution 
-- function of a Gaussian distribution with mean @0@ and standard deviation
-- @sigma@. It returns @x@ such that @P(x) = p@.
gaussianPInv :: Double -> Double -> Double
gaussianPInv = liftDouble2 gsl_cdf_gaussian_Pinv

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_gaussian_Pinv :: CDouble -> CDouble -> CDouble

-- | @gaussianPInv q sigma@ computes the inverse of the cumulative distribution 
-- function of a Gaussian distribution with mean @0@ and standard deviation
-- @sigma@. It returns @x@ such that @Q(x) = q@.
gaussianQInv :: Double -> Double -> Double
gaussianQInv = liftDouble2 gsl_cdf_gaussian_Qinv

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_gaussian_Qinv :: CDouble -> CDouble -> CDouble

-- | @getGaussian r sigma@ gets a normal random variable with mean
-- @0@ and standard deviation @sigma@.  
-- This uses the Box-Mueller algorithm.
getGaussian :: RNG -> Double -> IO Double
getGaussian = getGaussianHelp gsl_ran_gaussian

-- | @getGaussianZiggurat r sigma@ gets a normal random variable with mean
-- @0@ and standard deviation @sigma@.  
-- This uses the Marsaglia-Tsang ziggurat algorithm.
getGaussianZiggurat :: RNG -> Double -> IO Double
getGaussianZiggurat = getGaussianHelp gsl_ran_gaussian_ziggurat

-- | @getGaussianRatioMethod r sigma@ gets a normal random variable with mean
-- @0@ and standard deviation @sigma@.  
-- This uses the Kinderman-Monahan-Leva ratio method.
getGaussianRatioMethod:: RNG -> Double -> IO Double
getGaussianRatioMethod = getGaussianHelp gsl_ran_gaussian_ratio_method

getGaussianHelp :: (Ptr () -> CDouble -> IO CDouble) 
                -> RNG -> Double -> IO Double
getGaussianHelp ran_gaussian (MkRNG fptr) sigma  =
    let sigma' = realToFrac sigma
    in withForeignPtr fptr $ \ptr -> do
        x <- ran_gaussian ptr sigma'
        return $ realToFrac x

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_gaussian :: Ptr () -> CDouble -> IO CDouble
foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_gaussian_ziggurat :: Ptr () -> CDouble -> IO CDouble
foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_gaussian_ratio_method :: Ptr () -> CDouble -> IO CDouble

-- | @ugaussianPdf x@ computes the probabililty density p(x) for 
-- a Gaussian distribution with mean @0@ and standard deviation @1@.
ugaussianPdf :: Double -> Double
ugaussianPdf = liftDouble gsl_ran_ugaussian_pdf

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_ugaussian_pdf :: CDouble -> CDouble

-- | @ugaussianP x@ computes the cumulative distribution function P(x) for 
-- a Gaussian distribution with mean @0@ and standard deviation @1@.
ugaussianP :: Double -> Double
ugaussianP = liftDouble gsl_cdf_ugaussian_P

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_ugaussian_P :: CDouble -> CDouble

-- | @ugaussianQ x@ computes the cumulative distribution function Q(x) for 
-- a Gaussian distribution with mean @0@ and standard deviation @1@.
ugaussianQ :: Double -> Double
ugaussianQ = liftDouble gsl_cdf_ugaussian_Q

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_ugaussian_Q :: CDouble -> CDouble

-- | @ugaussianPInv p@ computes the inverse of the cumulative distribution 
-- function of a Gaussian distribution with mean @0@ and standard deviation
-- @1@. It returns @x@ such that @P(x) = p@.
ugaussianPInv :: Double -> Double
ugaussianPInv = liftDouble gsl_cdf_ugaussian_Pinv

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_ugaussian_Pinv :: CDouble -> CDouble

-- | @ugaussianPInv q@ computes the inverse of the cumulative distribution 
-- function of a Gaussian distribution with mean @0@ and standard deviation
-- @1@. It returns @x@ such that @Q(x) = q@.
ugaussianQInv :: Double -> Double
ugaussianQInv = liftDouble gsl_cdf_ugaussian_Qinv

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_ugaussian_Qinv :: CDouble -> CDouble

-- | @getUGaussian r@ gets a normal random variable with mean
-- @0@ and standard deviation @1@.  
-- This uses the Box-Mueller algorithm.
getUGaussian :: RNG -> IO Double
getUGaussian = getUGaussianHelp gsl_ran_ugaussian

-- | @getUGaussianZiggurat r@ gets a normal random variable with mean
-- @0@ and standard deviation @1@.  
-- This uses the Marsaglia-Tsang ziggurat algorithm.
getUGaussianZiggurat :: RNG -> IO Double
getUGaussianZiggurat = getUGaussianHelp gsl_ran_ugaussian_ziggurat

-- | @getUGaussianRatioMethod r@ gets a normal random variable with mean
-- @0@ and standard deviation @1@.  
-- This uses the Kinderman-Monahan-Leva ratio method.
getUGaussianRatioMethod:: RNG -> IO Double
getUGaussianRatioMethod = getUGaussianHelp gsl_ran_ugaussian_ratio_method
    
getUGaussianHelp :: (Ptr () -> IO CDouble) 
                -> RNG -> IO Double
getUGaussianHelp ran_ugaussian (MkRNG fptr)  =
    withForeignPtr fptr $ \ptr -> do
        x <- ran_ugaussian ptr
        return $ realToFrac x

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_ugaussian :: Ptr () -> IO CDouble
foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_ugaussian_ziggurat :: Ptr () -> IO CDouble
foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_ugaussian_ratio_method :: Ptr () -> IO CDouble
    

-- | @flatPdf x a b@ computes the probability density @p(x)@ at @x@ for
-- a uniform distribution from @a@ to @b@.
flatPdf :: Double -> Double -> Double -> Double
flatPdf = liftDouble3 gsl_ran_flat_pdf

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_flat_pdf :: CDouble -> CDouble -> CDouble -> CDouble

-- | @flatP x a b@ computes the cumulative distribution function @P(x)@.
flatP :: Double -> Double -> Double -> Double
flatP = liftDouble3 gsl_cdf_flat_P

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_flat_P :: CDouble -> CDouble -> CDouble -> CDouble

-- | @flatQ x a b@ computes the cumulative distribution function @Q(x)@.
flatQ :: Double -> Double -> Double -> Double
flatQ = liftDouble3 gsl_cdf_flat_Q

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_flat_Q :: CDouble -> CDouble -> CDouble -> CDouble

-- | @flatPInv p a b@ computes the inverse of the cumulative distribution
-- and returns @x@ so that function @P(x) = p@.
flatPInv :: Double -> Double -> Double -> Double
flatPInv = liftDouble3 gsl_cdf_flat_Pinv

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_flat_Pinv :: CDouble -> CDouble -> CDouble -> CDouble

-- | @flatQInv q a b@ computes the inverse of the cumulative distribution
-- and returns @x@ so that function @Q(x) = q@.
flatQInv :: Double -> Double -> Double -> Double
flatQInv = liftDouble3 gsl_cdf_flat_Qinv

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_flat_Qinv :: CDouble -> CDouble -> CDouble -> CDouble

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


poissonPdf :: Int -> Double -> Double
poissonPdf = undefined

poissonP :: Int -> Double -> Double
poissonP = undefined

poissonQ :: Int -> Double -> Double
poissonQ = undefined

--poissonPInv :: Double -> Double -> Int
--poissonQInv :: Double -> Double -> Int

    
-- | @getPoisson r mu@ gets a poisson random variable with mean @mu@.
getPoisson :: RNG -> Double -> IO Int
getPoisson (MkRNG fptr) mu =
    let mu' = realToFrac mu
    in withForeignPtr fptr $ \ptr -> do
        x <- gsl_ran_poisson ptr mu' 
        return $ (fromInteger . toInteger) x
                    
foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_poisson :: Ptr () -> CDouble -> IO CUInt



    
liftDouble :: (CDouble -> CDouble) 
           -> Double -> Double
liftDouble f x =
    realToFrac $ f (realToFrac x)

liftDouble2 :: (CDouble -> CDouble -> CDouble) 
           -> Double -> Double -> Double
liftDouble2 f x y =
    realToFrac $ f (realToFrac x) (realToFrac y)

liftDouble3 :: (CDouble -> CDouble -> CDouble -> CDouble) 
           -> Double -> Double -> Double -> Double
liftDouble3 f x y z =
    realToFrac $ f (realToFrac x) (realToFrac y) (realToFrac z)

