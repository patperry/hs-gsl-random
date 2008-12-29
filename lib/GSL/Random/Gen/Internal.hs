{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module     : GSL.Random.Gen.Internal
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module GSL.Random.Gen.Internal (
    -- * Data types
    RNG(..),
    RNGType,
    
    -- * Initializing
    newRNG,
    setSeed,
    
    -- * Sampling
    getSample,
    getUniform,
    getUniformPos,
    getUniformInt,
    
    -- * Auxiliary functions
    getName,
    getMax,
    getMin,
    getSize,
    getState,
    setState,
    
    -- * Copying state
    copyRNG,
    cloneRNG,

    -- * Algorithms
    mt19937,
    
    ) where
       
import Control.Monad         ( liftM )
import Data.Maybe            ( fromJust ) 
import Data.Word             ( Word8, Word64 )
import Foreign.C.Types       ( CULong, CSize, CInt, CDouble )
import Foreign.C.String      ( CString, withCAString, peekCAString )
import Foreign.ForeignPtr    ( ForeignPtr, newForeignPtr, withForeignPtr )
import Foreign.Marshal.Array ( peekArray, pokeArray, advancePtr )
import Foreign.Ptr           ( Ptr, FunPtr, castPtr, nullPtr )
import Foreign.Storable      ( peek )
import System.IO.Unsafe      ( unsafePerformIO )

newtype RNG     = MkRNG (ForeignPtr ())
newtype RNGType = MkRNGType (Ptr ())


--------------------------- Initialization ----------------------------------

-- | Allocate a new random number generator of the given type and initialize
-- it with the default seed.
newRNG :: RNGType -> IO RNG
newRNG t = do
    ptr  <- gsl_rng_alloc t
    fptr <- newForeignPtr p_gsl_rng_free ptr
    return $! MkRNG fptr

foreign import ccall unsafe "gsl/gsl_rng.h"
    gsl_rng_alloc :: RNGType -> IO (Ptr ())

foreign import ccall unsafe "gsl/gsl_rng.h &gsl_rng_free"
    p_gsl_rng_free :: FunPtr (Ptr () -> IO ())

-- | Seed the generator with the given value.
setSeed :: RNG -> Word64 -> IO ()
setSeed (MkRNG fptr) seed =
    let seed' = (fromInteger . toInteger) seed
    in withForeignPtr fptr $ flip gsl_rng_set seed'

foreign import ccall unsafe "gsl/gsl_rng.h"
    gsl_rng_set :: Ptr () -> CULong -> IO ()
    

--------------------------- Sampling ----------------------------------------

-- | Returns a value uniform in [rngMin, rngMax]
getSample :: RNG -> IO Word64
getSample (MkRNG fptr) =
    withForeignPtr fptr $ \ptr ->
        gsl_rng_get ptr >>= return . fromInteger . toInteger
    
foreign import ccall unsafe "gsl/gsl_rng.h"
    gsl_rng_get :: Ptr () -> IO CULong

-- | Returns a value uniform on [0,1)
getUniform :: RNG -> IO Double
getUniform (MkRNG fptr) =
    withForeignPtr fptr $ \ptr ->
        gsl_rng_uniform ptr >>= return . realToFrac
    
foreign import ccall unsafe "gsl/gsl_rng.h"
    gsl_rng_uniform :: Ptr () -> IO CDouble

-- | Returns a value uniform on (0,1)
getUniformPos :: RNG -> IO Double
getUniformPos (MkRNG fptr) =
    withForeignPtr fptr $ \ptr ->
        gsl_rng_uniform_pos ptr >>= return . realToFrac

foreign import ccall unsafe "gsl/gsl_rng.h"
    gsl_rng_uniform_pos :: Ptr () -> IO CDouble

-- | Returns an integer uniform on [0,n-1].  @n@ must be greater than @0@.
getUniformInt :: RNG -> Int -> IO Int
getUniformInt (MkRNG fptr) n 
    | n <= 0 =
        ioError $ userError $ 
            "rngUnifInt: expected \"n\" to be greater than 0" ++
            " but got `" ++ show n ++ "' instead."
    | otherwise =
        let n' = (fromInteger . toInteger) n
        in withForeignPtr fptr $ \ptr ->
            gsl_rng_uniform_int ptr n' >>= return . fromInteger . toInteger
    
foreign import ccall unsafe "gsl/gsl_rng.h"
    gsl_rng_uniform_int :: Ptr () -> CULong -> IO CULong


--------------------------- Auxiliary Functions -----------------------------

-- | Get the name of the generator.
getName :: RNG -> IO String
getName (MkRNG fptr) =
    withForeignPtr fptr $ \ptr ->
         peekCAString (gsl_rng_name ptr)
    
foreign import ccall unsafe "gsl/gsl_rng.h"
    gsl_rng_name :: Ptr () -> CString

-- | Get the largest value that the generator can return.
getMax :: RNG -> IO Word64
getMax (MkRNG fptr) =
    withForeignPtr fptr $ \ptr ->
        (return . fromInteger . toInteger) (gsl_rng_max ptr)
        
foreign import ccall unsafe "gsl/gsl_rng.h"
    gsl_rng_max :: Ptr () -> CULong

-- | Get the smallest value that the generator can return.
getMin :: RNG -> IO Word64
getMin (MkRNG fptr) =
    withForeignPtr fptr $ \ptr ->
        (return . fromInteger . toInteger) (gsl_rng_min ptr)
        
foreign import ccall unsafe "gsl/gsl_rng.h"
    gsl_rng_min :: Ptr () -> CULong

-- | Get the size of the generator state, in bytes.
getSize :: RNG -> IO Word64
getSize (MkRNG fptr) =
    withForeignPtr fptr $ \ptr ->
        (return . fromInteger . toInteger) (gsl_rng_size ptr)

foreign import ccall unsafe "gsl/gsl_rng.h"
    gsl_rng_size :: Ptr () -> CSize

-- | Get the generator state.
getState :: RNG -> IO [Word8]
getState rng@(MkRNG fptr) = do
    n <- liftM (fromInteger . toInteger) (getSize rng)
    withForeignPtr fptr $ \ptr ->
        peekArray n (gsl_rng_state ptr)
        
foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_rng_state :: Ptr () -> Ptr Word8

-- | Set the generator state.  The input array should have size equal
-- to @getSize@ of the generator; otherwise, strange things will happen.
setState :: RNG -> [Word8] -> IO ()
setState (MkRNG fptr) state = do
    withForeignPtr fptr $ \ptr ->
        pokeArray (gsl_rng_state ptr) state

--------------------------- Copying State -----------------------------------

-- | @copyRNG dst src@ copies the state from one generator to another.  The
--   two generators must have the same type.
copyRNG :: RNG -> RNG -> IO ()
copyRNG (MkRNG fdst) (MkRNG fsrc) =
    withForeignPtr fdst $ \dst ->
        withForeignPtr fsrc $ \src ->
            gsl_rng_memcpy dst src
            
foreign import ccall unsafe "gsl/gsl_rng.h"
    gsl_rng_memcpy :: Ptr () -> Ptr () -> IO ()

-- | Allocate a new random number generator that is an exact copy of another
-- generator
cloneRNG :: RNG -> IO RNG
cloneRNG (MkRNG fptr) =
    withForeignPtr fptr $ \ptr -> do
        ptr'  <- gsl_rng_clone ptr
        fptr' <- newForeignPtr p_gsl_rng_free ptr'
        ptr' `seq` return $! MkRNG fptr'

foreign import ccall unsafe "gsl/gsl_rng.h"
    gsl_rng_clone :: Ptr () -> IO (Ptr ())


--------------------------- Algorithms --------------------------------------


mt19937 :: RNGType
mt19937 = 
    unsafePerformIO $
        withCAString "mt19937" $ \name ->
            liftM fromJust $ getRngType name
{-# NOINLINE mt19937 #-}

getRngType :: CString -> IO (Maybe RNGType)
getRngType name =
    go gsl_rng_types_setup
    where
        go :: Ptr (Ptr ()) -> IO (Maybe RNGType)
        go types = do
            t <- peek types
            if t == nullPtr 
              then
                return Nothing
                
              else do
                name' <- peek (castPtr t)
                cmp   <- c_strcmp name name'
                
                if cmp == 0
                  then return $ Just (MkRNGType t)
                  else go $ types `advancePtr` 1
                    


foreign import ccall unsafe "string.h strcmp"
    c_strcmp :: CString -> CString -> IO CInt

foreign import ccall unsafe "gsl/gsl_randist.h"
    gsl_rng_types_setup :: Ptr (Ptr ())
