{-# LANGUAGE ForeignFunctionInterface #-}

module BioInf.ViennaRNA.Bindings.FFI.CoFold
  ( ffiCoFold
  ) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import GHC.Float
import Unsafe.Coerce

import BioInf.ViennaRNA.Bindings.FFI.Utils



#include <ViennaRNA/fold.h>
#include <ViennaRNA/cofold.h>

-- | TODO set cut point (this will set a global viariable)

ffiCoFold :: Int -> String -> IO (Double,String)
ffiCoFold cp inp = withCAString inp $ \cinp ->
                   withCAString inp $ \struc -> do
  setCutPoint cp
  e <- {#call cofold #} cinp struc
  s <- peekCAString struc
  return (cf2d e, s)

ffiCoEnergyOfStructure :: Int -> String -> String -> Int -> IO Double
ffiCoEnergyOfStructure cp inp struc verb =
  withCAString inp   $ \i ->
  withCAString struc $ \s ->
    setCutPoint cp
    >>  {#call energy_of_structure #} i s (fromIntegral verb :: CInt)
    >>= (return . cf2d)

