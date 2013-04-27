{-# LANGUAGE ForeignFunctionInterface #-}

module BioInf.ViennaRNA.Bindings.FFI.PartFunc
  ( ffiPartitionFunction
  ) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import GHC.Float
import qualified Data.Array.IArray as A
import Unsafe.Coerce

import BioInf.ViennaRNA.Bindings.FFI.Utils



#include <ViennaRNA/part_func.h>

ffiPartitionFunction :: String -> IO (Double,String,A.Array (Int,Int) Double)
ffiPartitionFunction i =
  withCAString i $ \ci ->
  withCAString i $ \cs -> do
  let n = length i
  let z = n * (n+1) `div` 2 +1
  e  <- {#call pf_fold #} ci cs
  s  <- peekCAString cs
  bp <- {#call export_bppm #}
  xs <- peekArray z (bp :: Ptr CDouble)
  let ar = A.accumArray (const id) 0 ((1,1),(n,n)) $ zip [ (ii,jj) | ii <- [n,n-1..1], jj <- [13,12..ii]] (drop 1 $ map unsafeCoerce xs)
  return (cf2d e, s, ar)

