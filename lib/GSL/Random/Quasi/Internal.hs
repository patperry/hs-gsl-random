{-# LANGUAGE ForeignFunctionInterface #-}
----------------------------------------------------------------------------
-- |
-- Module     : GSL.Random.Quasi.Internal
-- Copyright  : Copyright (c) 2009 , Tracy Wadleigh
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module GSL.Random.Quasi.Internal (
    -- * Data types
    QRNG(..),
    QRNGType,

    -- * Initializing
    newQRNG,
    reinitialize,

    -- * Sampling
    getSample,
    getListSample,

    -- * Auxiliary functions
    getName,
    getDimension,
    getSize,
    getState,
    setState,

    -- * Copying state
    copyQRNG,
    cloneQRNG,

    -- * Algorithms
    niederreiter,
    sobol,
    halton,
    reverseHalton,
    maxDimension,

    ) where

import Control.Monad
import Foreign
import Foreign.C.String
import Foreign.C.Types

newtype QRNG     = MkQRNG (ForeignPtr QRNG)
newtype QRNGType = MkQRNGType (Ptr QRNGType)


--------------------------- Initialization ---------------------------------

-- | Allocate a new quasi-random number generator of the given type,
-- generating points with the given number of dimensions.
newQRNG :: QRNGType -> Int -> IO QRNG
newQRNG t dim = do
    ptr <- gsl_qrng_alloc t (fromIntegral dim)
    fptr <- newForeignPtr p_gsl_qrng_free ptr
    return $! MkQRNG fptr

foreign import ccall unsafe "gsl/gsl_qrng.h"
    gsl_qrng_alloc :: QRNGType -> CUInt -> IO (Ptr QRNG)

foreign import ccall unsafe "gsl/gsl_qrng.h &gsl_qrng_free"
    p_gsl_qrng_free :: FunPtr (Ptr QRNG -> IO ())

-- | Reset the generator to the beginning of its sequence.
reinitialize :: QRNG -> IO ()
reinitialize (MkQRNG fptr) = withForeignPtr fptr $ gsl_qrng_init

foreign import ccall unsafe "gsl/gsl_qrng.h"
    gsl_qrng_init :: Ptr QRNG -> IO ()


--------------------------- Sampling ---------------------------------------

-- | Stores the next point from the generator in the given buffer. The
-- space available in the buffer must match the dimension of the generator.
-- The components of the sample will each lie in the range (0,1).
getSample :: QRNG -> Ptr Double -> IO ()
getSample (MkQRNG fptr) smplPtr =
    withForeignPtr fptr $ \ptr ->
        gsl_qrng_get ptr smplPtr >> (return ())

foreign import ccall unsafe "gsl/gsl_qrng.h"
    gsl_qrng_get :: Ptr QRNG -> Ptr Double -> IO CInt

-- | Gets the next sample point as a list.
getListSample :: QRNG -> IO [Double]
getListSample qrng = do
    dim <- getDimension qrng >>= (return . fromIntegral)
    allocaArray dim $ \ptr -> do
        getSample qrng ptr
        peekArray dim ptr


--------------------------- Auxiliary Functions ----------------------------

-- | Get the name of the generator.
getName :: QRNG -> IO String
getName (MkQRNG fptr) =
    withForeignPtr fptr $ \ptr ->
        peekCAString (gsl_qrng_name ptr)

foreign import ccall unsafe "gsl/gsl_qrng.h"
    gsl_qrng_name :: Ptr QRNG -> CString

-- | The dimension of the sequence.
getDimension :: QRNG -> IO Int
getDimension (MkQRNG fptr) =
    withForeignPtr fptr $ \ptr ->
        gsl_qrng_get_dimension ptr >>= (return . fromIntegral)

foreign import ccall unsafe
    gsl_qrng_get_dimension :: Ptr QRNG -> IO CUInt

-- | Get the size of the generator state, in bytes.
getSize :: QRNG -> IO Word64
getSize (MkQRNG fptr) =
    withForeignPtr fptr $ \ptr ->
        (return . fromInteger . toInteger) (gsl_qrng_size ptr)

foreign import ccall unsafe "gsl/gsl_qrng.h"
    gsl_qrng_size :: Ptr QRNG -> CSize

-- | Get the generator state.
getState :: QRNG -> IO [Word8]
getState qrng@(MkQRNG fptr) = do
    n <- liftM (fromInteger . toInteger) (getSize qrng)
    withForeignPtr fptr $ \ptr ->
        peekArray n (gsl_qrng_state ptr)

foreign import ccall unsafe "gsl/gsl_qrng.h"
    gsl_qrng_state :: Ptr QRNG -> Ptr Word8

-- | Set the generator state.  The input array should have size equal to
-- @getSize@ of the generator; otherwise, strange things will happen.
setState :: QRNG -> [Word8] -> IO ()
setState (MkQRNG fptr) state = do
    withForeignPtr fptr $ \ptr ->
        pokeArray (gsl_qrng_state ptr) state


--------------------------- Copying state -----------------------------------

-- | @copyQRNG dst src@ copies the state from one generator to another. The
--   two generators must have the same type.
copyQRNG :: QRNG -> QRNG -> IO ()
copyQRNG (MkQRNG fdst) (MkQRNG fsrc) =
    withForeignPtr fdst $ \dst ->
        withForeignPtr fsrc $ \src ->
            gsl_qrng_memcpy dst src

foreign import ccall unsafe "gsl/gsl_qrng.h"
    gsl_qrng_memcpy :: Ptr QRNG -> Ptr QRNG -> IO ()

-- | Allocate a new quasi-random number generator that is exact copy of
-- another generator.
cloneQRNG :: QRNG -> IO QRNG
cloneQRNG (MkQRNG fptr) =
    withForeignPtr fptr $ \ptr -> do
        ptr' <- gsl_qrng_clone ptr
        fptr' <- newForeignPtr p_gsl_qrng_free ptr'
        ptr' `seq` return $! MkQRNG fptr'

foreign import ccall unsafe "gsl/gsl_qrng.h"
    gsl_qrng_clone :: Ptr QRNG -> IO (Ptr QRNG)


--------------------------- Algorithms --------------------------------------

niederreiter :: QRNGType
niederreiter = unsafePerformIO getNiederreiter

sobol :: QRNGType
sobol = unsafePerformIO getSobol

halton :: QRNGType
halton = unsafePerformIO getHalton

reverseHalton :: QRNGType
reverseHalton = unsafePerformIO getReverseHalton

getNiederreiter :: IO QRNGType
getNiederreiter = gsl_qrng_get_niederreiter_2 >>= (return . MkQRNGType)

getSobol :: IO QRNGType
getSobol = gsl_qrng_get_sobol >>= (return . MkQRNGType)

getHalton :: IO QRNGType
getHalton = gsl_qrng_get_halton >>= (return . MkQRNGType)

getReverseHalton :: IO QRNGType
getReverseHalton = gsl_qrng_get_reversehalton >>= (return . MkQRNGType)

foreign import ccall unsafe
    gsl_qrng_get_niederreiter_2 :: IO (Ptr QRNGType)

foreign import ccall unsafe
    gsl_qrng_get_sobol :: IO (Ptr QRNGType)

foreign import ccall unsafe
    gsl_qrng_get_halton :: IO (Ptr QRNGType)

foreign import ccall unsafe
    gsl_qrng_get_reversehalton :: IO (Ptr QRNGType)

-- | The maximum dimension of samples that the given generator supports.
maxDimension :: QRNGType -> Int
maxDimension = unsafePerformIO . getMaxDimension

getMaxDimension :: QRNGType -> IO Int
getMaxDimension t =
    gsl_qrng_get_max_dimension t >>= (return . fromIntegral)

foreign import ccall unsafe
    gsl_qrng_get_max_dimension :: QRNGType -> IO CUInt