
module BioInf.ViennaRNA.Bindings.FFI.Utils where

import Foreign.C.Types
import GHC.Float
import Unsafe.Coerce
import Foreign.Ptr
import Foreign.Storable



cf2d :: CFloat -> Double
cf2d = float2Double . unsafeCoerce

cd2d :: CDouble -> Double
cd2d = unsafeCoerce

foreign import ccall "fold.h &cut_point" cut_point :: Ptr CInt

setCutPoint :: Int -> IO ()
setCutPoint = poke cut_point . unsafeCoerce

b2ci :: Bool -> CInt
b2ci True  = 1
b2ci False = 0

data RNAfoldOptions = RNAfoldOptions
  { _fomfe          :: !Bool
  , _foensemble     :: !Bool
  , _focentroid     :: !Bool
  , _fotemperature  :: !Double
  , _fodangles      :: !Int
  , _fonogu         :: !Bool
  , _fonolp         :: !Bool
  }

