
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
import qualified Data.Array.MArray as AM
import qualified Data.Array.Storable as AS
import qualified Data.Array.Unboxed as AU
import qualified Data.ByteString.Char8 as BS
import           Unsafe.Coerce

import BioInf.ViennaRNA.Bindings.FFI.Utils



ffi_RNAfold
  :: RNAfoldOptions
  -> ByteString   -- input
  -> IO ( Maybe (Double, ByteString) -- mfe
        , Maybe (Double, ByteString, AU.UArray (Int,Int) Double) -- ensemble
        , Maybe (Double, ByteString, Double) -- centroid
        )
ffi_RNAfold RNAfoldOptions{..} inp = do
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
          let l = if _foensemble then BS.length inp - 1 else 0
          bpp :: AS.StorableArray (Int,Int) Double <- AS.newArray ((0,0),(l,l)) 0
          AS.withStorableArray bpp $ \cbpparr -> do
          {#call ffiwrap_RNAfold #}
            (b2ci _fomfe) (b2ci _foensemble) (b2ci _focentroid) -- which parts to calculate
            (b2ci _fonolp) (fromIntegral _fodangles) (b2ci _fonogu) -- nolp, dangles, nogu
            (realToFrac _fotemperature)    -- temperature
            s1    -- input
            s2mfe -- mfe
            emfe
            s2ensemble  -- ensemble
            eensemble
            (unsafeCoerce cbpparr)
            s2centroid  -- centroid
            ecentroid
            cendist
          rmfe <- if _fomfe
            then do
              e <- cf2d <$> peek emfe
              str <- packCString s2mfe
              return $ Just (e,str)
            else return Nothing
          rensemble <- if _foensemble
            then do
              e <- cf2d <$> peek eensemble
              str <- packCString s2ensemble
              AS.freeze bpp >>= \ fbpp -> return $ Just (e,str,fbpp)
            else return Nothing
          rcentroid <- if _focentroid
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
  , double *probs // incoming array, large enough to write in base pairing probs, if withensemble == 1
  // centroid
  , char *s2centroid
  , double *ecentroid
  , double *centroiddistance
  );
#endc
