{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module BioInf.ViennaRNA.Bindings.FFI.CoFold
  ( ffiCoFold
  , ffiCoEnergyOfStructure
  , ffiCoPartitionFunction
  , CofoldF (..)
  ) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Float
import Unsafe.Coerce
import qualified Data.Array.IArray as A
import Foreign.Marshal.Array
import Control.Applicative
import Control.Monad

import BioInf.ViennaRNA.Bindings.FFI.Utils

import Debug.Trace



#include <ViennaRNA/fold.h>
#include <ViennaRNA/data_structures.h>
#include <ViennaRNA/cofold.h>
#include <ViennaRNA/part_func_co.h>

{#pointer *cofoldF as CofoldFPtr -> CofoldF #}

data CofoldF = CofoldF
  { f0ab :: Double
  , fab  :: Double
  , fcab :: Double
  , fa   :: Double
  , fb   :: Double
  }
  deriving (Show)

instance Storable CofoldF where
  sizeOf _ = {#sizeof cofoldF#}
  alignment _ = sizeOf (undefined :: CDouble)
  peek p = CofoldF <$> liftM unsafeCoerce ({# get cofoldF->F0AB #} p)
                   <*> liftM unsafeCoerce ({# get cofoldF->FAB  #} p)
                   <*> liftM unsafeCoerce ({# get cofoldF->FcAB #} p)
                   <*> liftM unsafeCoerce ({# get cofoldF->FA   #} p)
                   <*> liftM unsafeCoerce ({# get cofoldF->FB   #} p)

-- |

ffiCoFold :: Int -> String -> IO (Double,String)
ffiCoFold cp inp = withCAString inp $ \cinp ->
                   withCAString inp $ \struc -> do
  setCutPoint cp
  e <- {#call cofold #} cinp struc
  s <- peekCAString struc
  return (cf2d e, s)

-- |

ffiCoEnergyOfStructure :: Int -> String -> String -> Int -> IO Double
ffiCoEnergyOfStructure cp inp struc verb =
  withCAString inp   $ \i ->
  withCAString struc $ \s ->
    setCutPoint cp
    >>  {#call energy_of_structure #} i s (fromIntegral verb :: CInt)
    >>= (return . cf2d)

-- |

ffiCoPartitionFunction :: Int -> String -> IO (CofoldF,String,A.Array (Int,Int) Double)
ffiCoPartitionFunction cutpoint i =
  withCAString i $ \ci ->
  withCAString i $ \cs -> do
  setCutPoint cutpoint
  let n = length i
  let z = n * (n+1) `div` 2 +1
  eF <- co_pf_fold_p ci cs >>= peek -- {#call co_pf_fold #} ci cs
  s  <- peekCAString cs
  bp <- {#call export_co_bppm #}
  xs <- peekArray z (bp :: Ptr CDouble)
  let ar = A.accumArray (const id) 0 ((1,1),(n,n)) $ zip [ (ii,jj) | ii <- [n,n-1..1], jj <- [n,n-1..ii]] (drop 1 $ map unsafeCoerce xs)
  return (eF, s, ar)

foreign import ccall "co_pf_fold" co_pf_fold_p :: CString -> CString -> IO CofoldFPtr

#c
cofoldF * co_pf_fold_p (char * inp, char * str)
{ return & co_pf_fold (inp, str);
}
#endc

