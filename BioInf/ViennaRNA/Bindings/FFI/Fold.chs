{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module BioInf.ViennaRNA.Bindings.FFI.Fold
  ( ffiFold
  , ffiEnergyOfStructure
  , ffiEnergyOfCircStructure
  , ffiCircFold
  ) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import GHC.Float
import Unsafe.Coerce

import BioInf.ViennaRNA.Bindings.FFI.Utils



#include "fold.h"

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
    setCutPoint (-1)
    >>  {#call energy_of_structure #} i s (fromIntegral verb :: CInt)
    >>= (return . cf2d)

ffiEnergyOfCircStructure :: String -> String -> Int -> IO Double
ffiEnergyOfCircStructure inp struc verb =
  withCAString inp   $ \i ->
  withCAString struc $ \s ->
    setCutPoint (-1)
    >>  {#call energy_of_circ_structure #} i s (fromIntegral verb :: CInt)
    >>= (return . cf2d)

ffiCircFold :: String -> IO (Double,String)
ffiCircFold inp = withCAString inp $ \cinp ->
                  withCAString inp $ \struc -> do
  e <- {#call circfold #} cinp struc
  s <- peekCAString struc
  return (cf2d e, s)

