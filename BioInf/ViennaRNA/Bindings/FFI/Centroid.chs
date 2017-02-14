
module BioInf.ViennaRNA.Bindings.FFI.Centroid where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import GHC.Float
import Unsafe.Coerce

import BioInf.ViennaRNA.Bindings.FFI.Utils



#include "ViennaRNA/fold.h"



ffiCentroidTemp :: Double -> String -> IO (Double,String)
ffiCentroidTemp t inp =
  withCAString inp $ \cinp ->
  withCAString inp $ \struc -> do
  e <- fold_centroid_p (realToFrac t) cinp struc
  s <- peekCAString struc
  return (cd2d e, s)



foreign import ccall "ffiwrap_centroid_temp" fold_centroid_p :: CDouble -> CString -> CString -> IO CDouble

