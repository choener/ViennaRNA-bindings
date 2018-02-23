
module BioInf.ViennaRNA.Bindings.FFI.Utils where

import Data.Default.Class
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import GHC.Float
import Unsafe.Coerce



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

-- | Default for 'RNAfoldOptions' are to fold everything (@_fomfe, _foensemble,
-- _focentroid@), at @37 C@, with @_fodangles=2@, @_foonogu=False@, and
-- @_fonolp=True@.

data RNAfoldOptions = RNAfoldOptions
  { _fomfe          :: !Bool
  , _foensemble     :: !Bool
  , _focentroid     :: !Bool
  , _fotemperature  :: !Double
  , _fodangles      :: !Int
  , _fonogu         :: !Bool
  , _fonolp         :: !Bool
  }

instance Default RNAfoldOptions where
  def = RNAfoldOptions
    { _fomfe          = True
    , _foensemble     = True
    , _focentroid     = True
    , _fotemperature  = 37
    , _fodangles      = 2
    , _fonogu         = False
    , _fonolp         = True
    }

