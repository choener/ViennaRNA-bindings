{-# LANGUAGE ForeignFunctionInterface #-}

module BioInf.ViennaRNA.Bindings.FFI.PartFunc
  ( ffi_pf_fold
  , ffi_pf_circ_fold
  , ffi_pf_fold_constrained
  , ffi_pf_circ_fold_constrained
  ) where

import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           GHC.Float
import qualified Data.Array.IArray as A
import           Unsafe.Coerce

import BioInf.ViennaRNA.Bindings.FFI.Utils



#include "part_func.h"

ffi_pf_fold :: String -> IO (Double,String,A.Array (Int,Int) Double)
ffi_pf_fold i =
  withCAString i $ \ci ->
  withCAString i $ \cs -> do
  let n = length i
  let z = n * (n+1) `div` 2 +1
  e  <- {#call ffiwrap_pf_fold_constrained #} ci cs 0
  s  <- peekCAString cs
  bp <- {#call export_bppm #}
  xs <- peekArray z (bp :: Ptr CDouble)
  let ar = A.accumArray (const id) 0 ((1,1),(n,n)) $ zip [ (ii,jj) | ii <- [n,n-1..1], jj <- [n,n-1..ii]] (drop 1 $ map unsafeCoerce xs)
  return (cf2d e, s, ar)

ffi_pf_circ_fold :: String -> IO (Double,String,A.Array (Int,Int) Double)
ffi_pf_circ_fold i =
  withCAString i $ \ci -> do
  withCAString i $ \cs -> do
  let n = length i
  let z = n * (n+1) `div` 2 +1
  e  <- {#call pf_circ_fold #} ci cs
  s  <- peekCAString cs
  bp <- {#call export_bppm #}
  xs <- peekArray z (bp :: Ptr CDouble)
  let ar = A.accumArray (const id) 0 ((1,1),(n,n)) $ zip [ (ii,jj) | ii <- [n,n-1..1], jj <- [n,n-1..ii]] (drop 1 $ map unsafeCoerce xs)
  return (cf2d e, s, ar)

ffi_pf_fold_constrained :: String -> String -> IO (Double,String,A.Array (Int,Int) Double)
ffi_pf_fold_constrained i s =
  withCAString i $ \ci ->
  withCAString s $ \cs -> do
  let n = length i
  let z = n * (n+1) `div` 2 +1
  e  <- {#call ffiwrap_pf_fold_constrained #} ci cs 1
  s  <- peekCAString cs
  bp <- {#call export_bppm #}
  xs <- peekArray z (bp :: Ptr CDouble)
  let ar = A.accumArray (const id) 0 ((1,1),(n,n)) $ zip [ (ii,jj) | ii <- [n,n-1..1], jj <- [n,n-1..ii]] (drop 1 $ map unsafeCoerce xs)
  return (cf2d e, s, ar)

ffi_pf_circ_fold_constrained :: String -> String -> IO (Double,String,A.Array (Int,Int) Double)
ffi_pf_circ_fold_constrained i s =
  withCAString i $ \ci -> do
  withCAString i $ \cs -> do
  let n = length i
  let z = n * (n+1) `div` 2 +1
  e  <- {#call ffiwrap_pf_circ_fold_constrained #} ci cs 1
  s  <- peekCAString cs
  bp <- {#call export_bppm #}
  xs <- peekArray z (bp :: Ptr CDouble)
  let ar = A.accumArray (const id) 0 ((1,1),(n,n)) $ zip [ (ii,jj) | ii <- [n,n-1..1], jj <- [n,n-1..ii]] (drop 1 $ map unsafeCoerce xs)
  return (cf2d e, s, ar)

#c
float ffiwrap_pf_fold_constrained (const char *sequence, char *structure, int constrained);
float ffiwrap_pf_circ_fold_constrained (const char *sequence, char *structure, int constrained);
#endc

