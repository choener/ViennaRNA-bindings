
module BioInf.ViennaRNA.Bindings.FFI.CoFold
  ( ffiCoFold
  , ffiCoEnergyOfStructure
  , ffiCoPartitionFunction
  , ffiCoPartitionConstrained
  , CofoldF (..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString.Char8 (ByteString, useAsCString, packCString)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Float
import qualified Data.Array.IArray as A
import qualified Data.ByteString.Char8 as BS
import           Unsafe.Coerce

import           BioInf.ViennaRNA.Bindings.FFI.Utils



#include "ViennaRNA/cofold.h"
#include "ViennaRNA/data_structures.h"
#include "ViennaRNA/fold.h"
#include "ViennaRNA/part_func_co.h"

{#pointer *cofoldF as CofoldFPtr -> CofoldF #}

data CofoldF = CofoldF
  { f0ab :: {-# UNPACK #-} !Double
  , fab  :: {-# UNPACK #-} !Double
  , fcab :: {-# UNPACK #-} !Double
  , fa   :: {-# UNPACK #-} !Double
  , fb   :: {-# UNPACK #-} !Double
  }
  deriving (Show)

instance Storable CofoldF where
  sizeOf _ = {#sizeof cofoldF#}
  alignment _ = sizeOf (undefined :: CDouble)
  peek p = CofoldF <$> liftM realToFrac ({# get cofoldF->F0AB #} p)
                   <*> liftM realToFrac ({# get cofoldF->FAB  #} p)
                   <*> liftM realToFrac ({# get cofoldF->FcAB #} p)
                   <*> liftM realToFrac ({# get cofoldF->FA   #} p)
                   <*> liftM realToFrac ({# get cofoldF->FB   #} p)

-- |

ffiCoFold :: Int -> ByteString -> IO (Double,ByteString)
ffiCoFold cp inp = useAsCString inp $ \cinp ->
                   useAsCString inp $ \struc -> do
  setCutPoint cp
  e <- {#call cofold #} cinp struc
  s <- packCString struc
  return (cf2d e, s)

-- |

ffiCoEnergyOfStructure :: Int -> ByteString -> ByteString -> Int -> IO Double
ffiCoEnergyOfStructure cp inp struc verb =
  useAsCString inp   $ \i ->
  useAsCString struc $ \s ->
    setCutPoint cp
    >>  {#call energy_of_structure #} i s (fromIntegral verb :: CInt)
    >>= (return . cf2d)

-- |

ffiCoPartitionFunction :: Int -> ByteString -> IO (CofoldF,ByteString,A.Array (Int,Int) Double)
ffiCoPartitionFunction cutpoint i =
  useAsCString i $ \ci ->
  useAsCString i $ \cs ->
  alloca         $ \ptr -> do
  setCutPoint cutpoint
  let n = BS.length i
  let z = n * (n+1) `div` 2 +1
  eF <- co_pf_fold_p ptr ci cs >> peek ptr
  s  <- packCString cs
  bp <- {#call export_co_bppm #}
  xs <- peekArray z (bp :: Ptr CDouble)
  let ar = A.accumArray (const id) 0 ((1,1),(n,n)) $ zip [ (ii,jj) | ii <- [n,n-1..1], jj <- [n,n-1..ii]] (drop 1 $ map unsafeCoerce xs)
  return (eF, s, ar)

-- | Constrained partition function

ffiCoPartitionConstrained :: Int -> ByteString -> ByteString -> IO (CofoldF,ByteString,A.Array (Int,Int) Double)
ffiCoPartitionConstrained cutpoint sq st =
  useAsCString sq $ \csq ->
  useAsCString st $ \cst ->
  alloca          $ \ptr -> do
  setCutPoint cutpoint
  let n = BS.length sq
  let z = n * (n+1) `div` 2 +1
  eF <- co_pf_fold_constrained_p ptr csq cst 1 >> peek ptr
  s  <- packCString cst
  bp <- {#call export_co_bppm #}
  xs <- peekArray z (bp :: Ptr CDouble)
  let ar = A.accumArray (const id) 0 ((1,1),(n,n)) $ zip [ (ii,jj) | ii <- [n,n-1..1], jj <- [n,n-1..ii]] (drop 1 $ map unsafeCoerce xs)
  return (eF, s, ar)



foreign import ccall "ffiwrap_co_pf_fold" co_pf_fold_p :: CofoldFPtr -> CString -> CString -> IO ()

foreign import ccall "ffiwrap_co_pf_fold_constrained" co_pf_fold_constrained_p :: CofoldFPtr -> CString -> CString -> Int -> IO ()

