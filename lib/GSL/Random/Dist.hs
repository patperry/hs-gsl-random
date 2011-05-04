{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module     : GSL.Random.Dist
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--
-- Random number distributions. Functions for generating random variates and
-- computing their probability distributions.
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
    getUGaussianRatioMethod,

    -- * The Flat (Uniform) Distribution
    flatPdf,

    flatP,
    flatQ,
    flatPInv,
    flatQInv,

    getFlat,

    -- * The Exponential Distribution
    exponentialPdf,

    exponentialP,
    exponentialQ,
    exponentialPInv,
    exponentialQInv,

    getExponential,

    -- * The Levy alpha-Stable Distributions
    getLevy,
    getLevySkew,

    -- * The Poisson Distribution
    poissonPdf,

    poissonP,
    poissonQ,

    getPoisson,

    -- * The Cauchy Distribution
    getCauchy,

    cauchyPdf,
    cauchyP,
    cauchyQ,
    cauchyPInv,
    cauchyQInv,

    -- * The Beta Distribution
    getBeta,

    betaPdf,
    betaP,
    betaQ,
    betaPInv,
    betaQInv,

    -- * The Logistic Distribution
    getLogistic,

    logisticPdf,
    logisticP,
    logisticQ,
    logisticPInv,
    logisticQInv,

    -- * The Pareto Distribution
    getPareto,

    paretoPdf,
    paretoP,
    paretoQ,
    paretoPInv,
    paretoQInv,

    -- * The Weibull Distribution
    getWeibull,

    weibullPdf,
    weibullP,
    weibullQ,
    weibullPInv,
    weibullQInv,

    ) where

import Control.Applicative  ( (<$>) )
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
getGaussian = liftRan1 gsl_ran_gaussian

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_gaussian :: Ptr () -> CDouble -> IO CDouble

-- | @getGaussianZiggurat r sigma@ gets a normal random variable with mean
-- @0@ and standard deviation @sigma@.
-- This uses the Marsaglia-Tsang ziggurat algorithm.
getGaussianZiggurat :: RNG -> Double -> IO Double
getGaussianZiggurat = liftRan1 gsl_ran_gaussian_ziggurat

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_gaussian_ziggurat :: Ptr () -> CDouble -> IO CDouble

-- | @getGaussianRatioMethod r sigma@ gets a normal random variable with mean
-- @0@ and standard deviation @sigma@.
-- This uses the Kinderman-Monahan-Leva ratio method.
getGaussianRatioMethod:: RNG -> Double -> IO Double
getGaussianRatioMethod = liftRan1 gsl_ran_gaussian_ratio_method

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
getUGaussian = liftRan0 gsl_ran_ugaussian

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_ugaussian :: Ptr () -> IO CDouble

-- | @getUGaussianRatioMethod r@ gets a normal random variable with mean
-- @0@ and standard deviation @1@.
-- This uses the Kinderman-Monahan-Leva ratio method.
getUGaussianRatioMethod:: RNG -> IO Double
getUGaussianRatioMethod = liftRan0 gsl_ran_ugaussian_ratio_method

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
getFlat = liftRan2 gsl_ran_flat

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_flat :: Ptr () -> CDouble -> CDouble -> IO CDouble




-- | @getExponential r mu@ gets a random exponential with mean @mu@.
getExponential :: RNG -> Double -> IO Double
getExponential = liftRan1 gsl_ran_exponential

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_exponential :: Ptr () -> CDouble -> IO CDouble

-- | @exponentialPdf x mu@ computes the density at @x@ of an exponential
-- with mean @mu@.
exponentialPdf :: Double -> Double -> Double
exponentialPdf = liftDouble2 gsl_ran_exponential_pdf

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_exponential_pdf :: CDouble -> CDouble -> CDouble

exponentialP :: Double -> Double -> Double
exponentialP = liftDouble2 gsl_cdf_exponential_P

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_exponential_P :: CDouble -> CDouble -> CDouble

exponentialQ :: Double -> Double -> Double
exponentialQ = liftDouble2 gsl_cdf_exponential_Q

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_exponential_Q :: CDouble -> CDouble -> CDouble

exponentialPInv :: Double -> Double -> Double
exponentialPInv = liftDouble2 gsl_cdf_exponential_Pinv

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_exponential_Pinv :: CDouble -> CDouble -> CDouble

exponentialQInv :: Double -> Double -> Double
exponentialQInv = liftDouble2 gsl_cdf_exponential_Qinv

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_exponential_Qinv :: CDouble -> CDouble -> CDouble




-- | @getLevy r c alpha@ gets a variate from the Levy symmetric stable
-- distribution with scale @c@ and exponent @alpha@.  The algorithm only
-- works for @0 <= alpha <= 2@.
getLevy :: RNG -> Double -> Double -> IO (Double)
getLevy = liftRan2 gsl_ran_levy

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_levy :: Ptr () -> CDouble -> CDouble -> IO CDouble

-- | @getLevySkew r c alpha beta@ gets a variate from the Levy skew stable
-- distribution with scale @c@, exponent @alpha@, and skewness parameter
-- @beta@.  The skewness parameter must lie in the range @[-1,1]@.  The
-- algorithm only works for @0 <= alpha <= 2@.
getLevySkew :: RNG -> Double -> Double -> Double -> IO (Double)
getLevySkew = liftRan3 gsl_ran_levy_skew

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_levy_skew :: Ptr () -> CDouble -> CDouble -> CDouble -> IO CDouble





-- | @poissonPdf k mu@ evaluates the probability density @p(k)@ at @k@ for
-- a Poisson distribution with mean @mu@.
poissonPdf :: Int -> Double -> Double
poissonPdf k = liftDouble $ gsl_ran_poisson_pdf (fromIntegral k)

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_poisson_pdf :: CUInt -> CDouble -> CDouble

-- | @poissonP k mu@ evaluates the cumulative distribution function @P(k)@
-- at @k@ for a Poisson distribution with mean @mu@.
poissonP :: Int -> Double -> Double
poissonP k = liftDouble $ gsl_cdf_poisson_P (fromIntegral k)

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_poisson_P :: CUInt -> CDouble -> CDouble

-- | @poissonQ k mu@ evaluates the cumulative distribution function @Q(k)@
-- at @k@ for a Poisson distribution with mean @mu@.
poissonQ :: Int -> Double -> Double
poissonQ k = liftDouble $ gsl_cdf_poisson_Q (fromIntegral k)

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_poisson_Q :: CUInt -> CDouble -> CDouble


-- | @getPoisson r mu@ gets a poisson random variable with mean @mu@.
getPoisson :: RNG -> Double -> IO Int
getPoisson (MkRNG fptr) mu =
    let mu' = realToFrac mu
    in withForeignPtr fptr $ \ptr -> do
        x <- gsl_ran_poisson ptr mu'
        return $ fromIntegral x

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_poisson :: Ptr () -> CDouble -> IO CUInt




-- | @cauchyPdf x a@ evaluates the probability density @p(x)@ at @x@
-- for a Cauchy distribution with scale parameter @a@.  The density
-- is given by @p(x) dx = { 1 \over a\pi (1 + (x/a^2)) } dx@.
cauchyPdf :: Double -> Double -> Double
cauchyPdf = liftDouble2 gsl_ran_cauchy_pdf

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_cauchy_pdf :: CDouble -> CDouble -> CDouble

-- | @getCauchy r a@ gets a random cauchy with scale @a@.
getCauchy :: RNG -> Double -> IO Double
getCauchy = liftRan1 gsl_ran_cauchy

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_cauchy :: Ptr () -> CDouble -> IO CDouble

cauchyP :: Double -> Double -> Double
cauchyP = liftDouble2 gsl_cdf_cauchy_P

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_cauchy_P :: CDouble -> CDouble -> CDouble

cauchyQ :: Double -> Double -> Double
cauchyQ = liftDouble2 gsl_cdf_cauchy_Q

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_cauchy_Q :: CDouble -> CDouble -> CDouble

cauchyPInv :: Double -> Double -> Double
cauchyPInv = liftDouble2 gsl_cdf_cauchy_Pinv

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_cauchy_Pinv :: CDouble -> CDouble -> CDouble

cauchyQInv :: Double -> Double -> Double
cauchyQInv = liftDouble2 gsl_cdf_cauchy_Qinv

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_cauchy_Qinv :: CDouble -> CDouble -> CDouble




-- | @betaPdf x a b@ evaluates the probability density @p(x)@ at @x@
-- for a Beta distribution with parameters @a@ and @b@.  The density
-- is given by @p(x) dx = {\Gamma(a+b) \over \Gamma(a) \Gamma(b)} x^{a-1} (1-x)^{b-1} dx@
-- for @0 <= x <= 1@.
betaPdf :: Double -> Double -> Double -> Double
betaPdf = liftDouble3 gsl_ran_beta_pdf

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_beta_pdf :: CDouble -> CDouble -> CDouble -> CDouble

-- | @getBeta r a b@ gets a random beta with parameters @a@ and @b@.
getBeta :: RNG -> Double -> Double -> IO Double
getBeta = liftRan2 gsl_ran_beta

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_beta :: Ptr () -> CDouble -> CDouble -> IO CDouble

betaP :: Double -> Double -> Double -> Double
betaP = liftDouble3 gsl_cdf_beta_P

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_beta_P :: CDouble -> CDouble -> CDouble -> CDouble

betaQ :: Double -> Double -> Double -> Double
betaQ = liftDouble3 gsl_cdf_beta_Q

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_beta_Q :: CDouble -> CDouble -> CDouble -> CDouble

betaPInv :: Double -> Double -> Double -> Double
betaPInv = liftDouble3 gsl_cdf_beta_Pinv

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_beta_Pinv :: CDouble -> CDouble -> CDouble -> CDouble

betaQInv :: Double -> Double -> Double -> Double
betaQInv = liftDouble3 gsl_cdf_beta_Qinv

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_beta_Qinv :: CDouble -> CDouble -> CDouble -> CDouble




-- | @logisticPdf x a@ evaluates the probability density @p(x)@ at @x@
-- for a logistic distribution with scale parameter @a@.  The density
-- is given by @p(x) dx = { \exp(-x/a) \over a (1 + \exp(-x/a))^2 } dx@.
logisticPdf :: Double -> Double -> Double
logisticPdf = liftDouble2 gsl_ran_logistic_pdf

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_logistic_pdf :: CDouble -> CDouble -> CDouble

-- | @getLogistic r a@ gets a random logistic with scale @a@.
getLogistic :: RNG -> Double -> IO Double
getLogistic = liftRan1 gsl_ran_logistic

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_logistic :: Ptr () -> CDouble -> IO CDouble

logisticP :: Double -> Double -> Double
logisticP = liftDouble2 gsl_cdf_logistic_P

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_logistic_P :: CDouble -> CDouble -> CDouble

logisticQ :: Double -> Double -> Double
logisticQ = liftDouble2 gsl_cdf_logistic_Q

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_logistic_Q :: CDouble -> CDouble -> CDouble

logisticPInv :: Double -> Double -> Double
logisticPInv = liftDouble2 gsl_cdf_logistic_Pinv

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_logistic_Pinv :: CDouble -> CDouble -> CDouble

logisticQInv :: Double -> Double -> Double
logisticQInv = liftDouble2 gsl_cdf_logistic_Qinv

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_logistic_Qinv :: CDouble -> CDouble -> CDouble




-- | @paretoPdf x a b@ evaluates the probability density @p(x)@ at @x@
-- for a Pareto distribution with exponent @a@ and scale @b@.  The density
-- is given by @p(x) dx = (a/b) / (x/b)^{a+1} dx@ for @x >= b@.
paretoPdf :: Double -> Double -> Double -> Double
paretoPdf = liftDouble3 gsl_ran_pareto_pdf

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_pareto_pdf :: CDouble -> CDouble -> CDouble -> CDouble

-- | @getPareto r a b@ gets a random Pareto with exponent @a@ and scale @b@.
getPareto :: RNG -> Double -> Double -> IO Double
getPareto = liftRan2 gsl_ran_pareto

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_pareto :: Ptr () -> CDouble -> CDouble -> IO CDouble

paretoP :: Double -> Double -> Double -> Double
paretoP = liftDouble3 gsl_cdf_pareto_P

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_pareto_P :: CDouble -> CDouble -> CDouble -> CDouble

paretoQ :: Double -> Double -> Double -> Double
paretoQ = liftDouble3 gsl_cdf_pareto_Q

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_pareto_Q :: CDouble -> CDouble -> CDouble -> CDouble

paretoPInv :: Double -> Double -> Double -> Double
paretoPInv = liftDouble3 gsl_cdf_pareto_Pinv

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_pareto_Pinv :: CDouble -> CDouble -> CDouble -> CDouble

paretoQInv :: Double -> Double -> Double -> Double
paretoQInv = liftDouble3 gsl_cdf_pareto_Qinv

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_pareto_Qinv :: CDouble -> CDouble -> CDouble -> CDouble




-- | @weibullPdf x a b@ evaluates the probability density @p(x)@ at @x@
-- for a Weibull distribution with scale @a@ and exponent @b@.  The density
-- is given by @p(x) dx = {b \over a^b} x^{b-1}  \exp(-(x/a)^b) dx@ for @x >= 0@.
weibullPdf :: Double -> Double -> Double -> Double
weibullPdf = liftDouble3 gsl_ran_weibull_pdf

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_weibull_pdf :: CDouble -> CDouble -> CDouble -> CDouble

-- | @getWeibull r a b@ gets a random Weibull with scale @a@ and exponent @b@.
getWeibull :: RNG -> Double -> Double -> IO Double
getWeibull = liftRan2 gsl_ran_weibull

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_ran_weibull :: Ptr () -> CDouble -> CDouble -> IO CDouble

weibullP :: Double -> Double -> Double -> Double
weibullP = liftDouble3 gsl_cdf_weibull_P

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_weibull_P :: CDouble -> CDouble -> CDouble -> CDouble

weibullQ :: Double -> Double -> Double -> Double
weibullQ = liftDouble3 gsl_cdf_weibull_Q

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_weibull_Q :: CDouble -> CDouble -> CDouble -> CDouble

weibullPInv :: Double -> Double -> Double -> Double
weibullPInv = liftDouble3 gsl_cdf_weibull_Pinv

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_weibull_Pinv :: CDouble -> CDouble -> CDouble -> CDouble

weibullQInv :: Double -> Double -> Double -> Double
weibullQInv = liftDouble3 gsl_cdf_weibull_Qinv

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_cdf_weibull_Qinv :: CDouble -> CDouble -> CDouble -> CDouble




-- Helper functions

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



liftRan0 :: (Ptr () -> IO CDouble) -> RNG -> IO Double
liftRan0 ran_fn (MkRNG fptr) =
    withForeignPtr fptr $ \ptr ->
    realToFrac <$> ran_fn ptr

liftRan1 :: (Ptr () -> CDouble -> IO CDouble)
         -> RNG -> Double -> IO Double
liftRan1 ran_fn (MkRNG fptr) p =
    withForeignPtr fptr $ \ptr ->
    realToFrac <$> ran_fn ptr (realToFrac p)

liftRan2 :: (Ptr () -> CDouble -> CDouble -> IO CDouble)
         -> RNG -> Double -> Double -> IO Double
liftRan2 ran_fn (MkRNG fptr) p q =
    withForeignPtr fptr $ \ptr ->
    realToFrac <$> ran_fn ptr (realToFrac p) (realToFrac q)

liftRan3 :: (Ptr () -> CDouble -> CDouble -> CDouble -> IO CDouble)
         -> RNG -> Double -> Double -> Double -> IO Double
liftRan3 ran_fn (MkRNG fptr) p q r =
    withForeignPtr fptr $ \ptr ->
    realToFrac <$> ran_fn ptr (realToFrac p) (realToFrac q) (realToFrac r)
