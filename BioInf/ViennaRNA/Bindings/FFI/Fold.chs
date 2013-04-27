{-# LANGUAGE ForeignFunctionInterface #-}

module BioInf.ViennaRNA.Bindings.FFI.Fold
  ( ffiFold
  , ffiEnergyOfStructure
  ) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import GHC.Float
import Unsafe.Coerce

import BioInf.ViennaRNA.Bindings.FFI.Utils



#include <ViennaRNA/fold.h>

ffiFold :: String -> IO (Double,String)
ffiFold inp = withCAString inp $ \cinp ->
              withCAString inp $ \struc -> do
  e <- {#call fold #} cinp struc
  s <- peekCAString struc
  return (cf2d e, s)

ffiEnergyOfStructure :: String -> String -> Int -> IO Double
ffiEnergyOfStructure inp struc verb =
  withCAString inp   $ \i ->
  withCAString struc $ \s ->
  {#call energy_of_structure #} i s (fromIntegral verb :: CInt) >>= (return . cf2d)

