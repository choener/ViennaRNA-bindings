
module BioInf.ViennaRNA.Bindings.FFI.Utils where

import Foreign.C.Types
import GHC.Float
import Unsafe.Coerce



cf2d :: CFloat -> Double
cf2d = float2Double . unsafeCoerce

