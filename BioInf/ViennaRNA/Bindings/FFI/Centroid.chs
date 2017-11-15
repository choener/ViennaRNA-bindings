
module BioInf.ViennaRNA.Bindings.FFI.Centroid where

import Data.ByteString.Char8 as BS
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Float
import Unsafe.Coerce

import BioInf.ViennaRNA.Bindings.FFI.Utils



#include "ViennaRNA/fold.h"



ffiCentroidTemp :: Double -> ByteString -> IO (Double,ByteString,Double)
ffiCentroidTemp t inp =
  if BS.null inp
    then return (0,BS.empty,0)
    else useAsCString inp $ \cinp ->
         useAsCString inp $ \struc ->
         with (0 :: CDouble) $ \dist -> do
           e <- if BS.null inp then return 0 else fold_centroid_p (realToFrac t) cinp struc dist
           s <- packCString struc
           d <- peek dist
           return (cd2d e, s, cd2d d)



foreign import ccall "ffiwrap_centroid_temp" fold_centroid_p :: CDouble -> CString -> CString -> Ptr CDouble -> IO CDouble

