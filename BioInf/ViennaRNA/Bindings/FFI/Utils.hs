{-# LANGUAGE ForeignFunctionInterface #-}

module BioInf.ViennaRNA.Bindings.FFI.Utils where

import Foreign.C.Types
import GHC.Float
import Unsafe.Coerce
import Foreign.Ptr
import Foreign.Storable



cf2d :: CFloat -> Double
cf2d = float2Double . unsafeCoerce

foreign import ccall "fold.h &cut_point" cut_point :: Ptr CInt

setCutPoint :: Int -> IO ()
setCutPoint = poke cut_point . unsafeCoerce

