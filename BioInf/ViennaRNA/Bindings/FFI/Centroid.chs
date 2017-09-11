
module BioInf.ViennaRNA.Bindings.FFI.Centroid where

import Data.ByteString.Char8
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import GHC.Float
import Unsafe.Coerce

import BioInf.ViennaRNA.Bindings.FFI.Utils



#include "ViennaRNA/fold.h"



ffiCentroidTemp :: Double -> ByteString -> IO (Double,ByteString)
ffiCentroidTemp t inp =
  useAsCString inp $ \cinp ->
  useAsCString inp $ \struc -> do
  e <- fold_centroid_p (realToFrac t) cinp struc
  s <- packCString struc
  return (cd2d e, s)



foreign import ccall "ffiwrap_centroid_temp" fold_centroid_p :: CDouble -> CString -> CString -> IO CDouble

