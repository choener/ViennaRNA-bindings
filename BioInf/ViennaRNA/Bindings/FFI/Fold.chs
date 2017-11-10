
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

ffiFold :: ByteString -> IO (Double,ByteString)
ffiFold inp = useAsCString inp $ \cinp ->
              useAsCString inp $ \struc -> do
  e <- if BS.null inp then return 0 else {#call fold #} cinp struc
  s <- packCString struc
  return (cf2d e, s)

ffiEnergyOfStructure :: ByteString -> ByteString -> Int -> IO Double
ffiEnergyOfStructure inp struc verb =
  useAsCString inp   $ \i ->
  useAsCString struc $ \s ->
    setCutPoint (-1)
    >>  {#call energy_of_structure #} i s (fromIntegral verb :: CInt)
    >>= (return . cf2d)

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



ffiFoldTemp :: Double -> ByteString -> IO (Double,ByteString)
ffiFoldTemp t inp =
  useAsCString inp $ \cinp ->
  useAsCString inp $ \struc -> do
  e <- fold_temp_p (realToFrac t) cinp struc
  s <- packCString struc
  return (cf2d e, s)

ffiEnergyOfStructureTemp :: Double -> ByteString -> ByteString -> Int -> IO Double
ffiEnergyOfStructureTemp t inp struc verb =
  useAsCString inp   $ \i ->
  useAsCString struc $ \s ->
    setCutPoint (-1)
    >>  eos_temp_p (realToFrac t) i s (fromIntegral verb :: CInt)
    >>= (return . cf2d)


foreign import ccall "ffiwrap_fold_temp" fold_temp_p :: CFloat -> CString -> CString -> IO CFloat

foreign import ccall "ffiwrap_eos_temp" eos_temp_p :: CFloat -> CString -> CString -> CInt -> IO CFloat

