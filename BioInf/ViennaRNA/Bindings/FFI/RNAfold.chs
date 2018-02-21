
module BioInf.ViennaRNA.Bindings.FFI.RNAfold
  ( ffi_RNAfold
  ) where

import           Data.ByteString.Char8 (ByteString, packCString, useAsCString)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Float
import qualified Data.Array.IArray as A
import qualified Data.ByteString.Char8 as BS
import           Unsafe.Coerce

import BioInf.ViennaRNA.Bindings.FFI.Utils



--foreign import ccall "ffiwrap_rnafold" ffi_RNAfold
--  :: CInt -- with mfe
--  -> CInt -- with partfun
--  -> CInt -- with centroid
--  -> CInt -- no lp
--  -> CInt -- dangles
--  -> CInt -- no gu closure
--  -> CDouble  -- temperature
--  -> CString  -- input string
--  -> CString
--  -> Ptr CDouble
--  -> IO CDouble

ffi_RNAfold
  :: RNAfoldOptions
  -> ByteString   -- input
  -> IO ( Maybe (Double, ByteString) -- mfe
        , Maybe (Double, ByteString) -- ensemble
        , Maybe (Double, ByteString, Double) -- centroid
        )
ffi_RNAfold o inp = do
  if BS.null inp
    then return (Nothing, Nothing, Nothing)
    else useAsCString inp $ \s1 ->
         useAsCString inp $ \s2mfe  ->
         with (0 :: CFloat) $ \emfe ->
         useAsCString inp $ \s2ensemble ->
         with (0 :: CFloat) $ \eensemble ->
         useAsCString inp $ \s2centroid ->
         with (0 :: CDouble) $ \ecentroid ->
         with (0 :: CDouble) $ \cendist -> do
          {#call ffiwrap_RNAfold #}
            (b2ci $ _fomfe o) (b2ci $ _foensemble o) (b2ci $ _focentroid o) -- which parts to calculate
            0 2 0 -- lp, dangles, gu
            (realToFrac $ _fotemperature o)    -- temperature
            s1    -- input
            s2mfe -- mfe
            emfe
            s2ensemble  -- ensemble
            eensemble
            s2centroid  -- centroid
            ecentroid
            cendist
          rmfe <- if _fomfe o
            then do
              e <- cf2d <$> peek emfe
              str <- packCString s2mfe
              return $ Just (e,str)
            else return Nothing
          rensemble <- if _foensemble o
            then do
              e <- cf2d <$> peek eensemble
              str <- packCString s2ensemble
              return $ Just (e,str)
            else return Nothing
          rcentroid <- if _focentroid o
            then do
              e <- cd2d <$> peek ecentroid
              str <- packCString s2centroid
              d <- cd2d <$> peek cendist
              return $ Just (e,str,d)
            else return Nothing
          return
            (rmfe, rensemble, rcentroid)
{-
  if BS.null inp
    then return (0,BS.empty,0)
    else useAsCString inp $ \cinp ->
         useAsCString inp $ \struc ->
         with (0 :: CDouble) $ \dist -> do
           e <- if BS.null inp then return 0 else fold_centroid_p (realToFrac t) cinp struc dist
           s <- packCString struc
           d <- peek dist
           return (cd2d e, s, cd2d d)
-}

#c
int ffiwrap_RNAfold
  // input options
  ( int withmfe       // enable mfe calcualtion
  , int withpartfun   // enable partition function calculation
  , int withcentroid  // enable centroid calculation
  , int nolp
  , int dangles
  , int noguclosure
  // temperature
  , float temp
  // the actual input sequence
  , const char *s1
  // output options
  // mfe results
  , char *s2mfe
  , float *emfe
  // partition function string
  , char *s2ensemble  // this string is *not* a canonical secondary structure string
  , float *eensemble  // energy of the whole ensemble
  // centroid
  , char *s2centroid
  , double *ecentroid
  , double *centroiddistance
  );
#endc
