
module BioInf.ViennaRNA.Bindings.FFI.Fold where

import Data.ByteString.Char8 as BS
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import GHC.Float
import Unsafe.Coerce

import BioInf.ViennaRNA.Bindings.FFI.Utils



#include "ViennaRNA/fold.h"

-- | 

ffiFold ∷ ByteString → IO (Double,ByteString)
ffiFold = ffiFoldTemp True 37

ffiEnergyOfStructure :: ByteString -> ByteString -> Int -> IO Double
ffiEnergyOfStructure = ffiEnergyOfStructureTemp 37

ffiEnergyOfCircStructure :: ByteString -> ByteString -> Int -> IO Double
ffiEnergyOfCircStructure inp struc verb =
  useAsCString inp   $ \i ->
  useAsCString struc $ \s ->
    setCutPoint (-1)
    >>  {#call energy_of_circ_structure #} i s (fromIntegral verb :: CInt)
    >>= (return . cf2d)

ffiCircFold :: ByteString -> IO (Double,ByteString)
ffiCircFold inp = useAsCString inp $ \cinp ->
                  useAsCString inp $ \struc -> do
  e <- {#call circfold #} cinp struc
  s <- packCString struc
  return (cf2d e, s)



ffiFoldTemp :: Bool -> Double -> ByteString -> IO (Double,ByteString)
ffiFoldTemp noLP t inp
  | BS.null inp = return (0,BS.empty)
  | otherwise   =
      useAsCString inp $ \cinp ->
      useAsCString inp $ \struc -> do
      e <- fold_temp_p (fromEnum noLP) (realToFrac t) cinp struc
      s <- packCString struc
      return (cf2d e, s)

ffiEnergyOfStructureTemp :: Double -> ByteString -> ByteString -> Int -> IO Double
ffiEnergyOfStructureTemp t inp struc verb =
  useAsCString inp   $ \i ->
  useAsCString struc $ \s ->
    setCutPoint (-1)
    >>  eos_temp_p (realToFrac t) i s (fromIntegral verb :: CInt)
    >>= (return . cf2d)


foreign import ccall "ffiwrap_fold_temp" fold_temp_p ∷ CBool → CFloat → CString → CString → IO CFloat

foreign import ccall "ffiwrap_eos_temp" eos_temp_p :: CFloat -> CString -> CString -> CInt -> IO CFloat

