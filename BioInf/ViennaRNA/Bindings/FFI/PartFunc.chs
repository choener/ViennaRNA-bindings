
-- | The ffi wrappers in this module need to use the new vrna-style calls. The
-- current old calls are not multi-threaded.

module BioInf.ViennaRNA.Bindings.FFI.PartFunc
  ( ffi_pf_fold
  , ffi_pf_circ_fold
  , ffi_pf_fold_constrained
  , ffi_pf_circ_fold_constrained
  ) where

import           Data.ByteString.Char8 (ByteString, packCString, useAsCString)
import qualified Data.ByteString.Char8 as BS
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           GHC.Float
import qualified Data.Array.IArray as A
import           Unsafe.Coerce

import BioInf.ViennaRNA.Bindings.FFI.Utils



#include "ViennaRNA/part_func.h"

ffi_pf_fold :: ByteString -> IO (Double,ByteString,A.Array (Int,Int) Double)
ffi_pf_fold i =
  useAsCString i $ \ci ->
  useAsCString i $ \cs -> do
  let n = BS.length i
  let z = n * (n+1) `div` 2 +1
  e  <- {#call ffiwrap_pf_fold_constrained #} ci cs 0
  s  <- packCString cs
  bp <- {#call export_bppm #}
  xs <- peekArray z (bp :: Ptr CDouble)
  let ar = A.accumArray (const id) 0 ((1,1),(n,n)) $ zip [ (ii,jj) | ii <- [n,n-1..1], jj <- [n,n-1..ii]] (drop 1 $ map unsafeCoerce xs)
  return (cf2d e, s, ar)

ffi_pf_circ_fold :: ByteString -> IO (Double,ByteString,A.Array (Int,Int) Double)
ffi_pf_circ_fold i =
  useAsCString i $ \ci -> do
  useAsCString i $ \cs -> do
  let n = BS.length i
  let z = n * (n+1) `div` 2 +1
  e  <- {#call pf_circ_fold #} ci cs
  s  <- packCString cs
  bp <- {#call export_bppm #}
  xs <- peekArray z (bp :: Ptr CDouble)
  let ar = A.accumArray (const id) 0 ((1,1),(n,n)) $ zip [ (ii,jj) | ii <- [n,n-1..1], jj <- [n,n-1..ii]] (drop 1 $ map unsafeCoerce xs)
  return (cf2d e, s, ar)

ffi_pf_fold_constrained :: ByteString -> ByteString -> IO (Double,ByteString,A.Array (Int,Int) Double)
ffi_pf_fold_constrained i s =
  useAsCString i $ \ci ->
  useAsCString s $ \cs -> do
  let n = BS.length i
  let z = n * (n+1) `div` 2 +1
  e  <- {#call ffiwrap_pf_fold_constrained #} ci cs 1
  s  <- packCString cs
  bp <- {#call export_bppm #}
  xs <- peekArray z (bp :: Ptr CDouble)
  let ar = A.accumArray (const id) 0 ((1,1),(n,n)) $ zip [ (ii,jj) | ii <- [n,n-1..1], jj <- [n,n-1..ii]] (drop 1 $ map unsafeCoerce xs)
  return (cf2d e, s, ar)

ffi_pf_circ_fold_constrained :: ByteString -> ByteString -> IO (Double,ByteString,A.Array (Int,Int) Double)
ffi_pf_circ_fold_constrained i s =
  useAsCString i $ \ci -> do
  useAsCString i $ \cs -> do
  let n = BS.length i
  let z = n * (n+1) `div` 2 +1
  e  <- {#call ffiwrap_pf_circ_fold_constrained #} ci cs 1
  s  <- packCString cs
  bp <- {#call export_bppm #}
  xs <- peekArray z (bp :: Ptr CDouble)
  let ar = A.accumArray (const id) 0 ((1,1),(n,n)) $ zip [ (ii,jj) | ii <- [n,n-1..1], jj <- [n,n-1..ii]] (drop 1 $ map unsafeCoerce xs)
  return (cf2d e, s, ar)

#c
float ffiwrap_pf_fold_constrained (const char *sequence, char *structure, int constrained);
float ffiwrap_pf_circ_fold_constrained (const char *sequence, char *structure, int constrained);
#endc

