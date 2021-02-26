{-# Language QuasiQuotes     #-}
{-# Language TemplateHaskell #-}
{-# Language BangPatterns    #-}
{-# Language ViewPatterns    #-}

module BioInf.ViennaRNA.Bindings.Inline where

import Data.ByteString.Char8 as BS
import qualified Language.C.Inline as C
import Foreign.C.Types
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Internal as BI
import Foreign.ForeignPtr.Unsafe



C.context (C.baseCtx <> C.bsCtx)
C.include "../C/ViennaRNA/hairpin_loops.h"
C.include "../C/ViennaRNA/interior_loops.h"
C.include "../C/ViennaRNA/fold.h"

-- | Create a default fold compound.

mkFoldCompound :: BS.ByteString -> IO (Ptr ())
mkFoldCompound inp = do
  c <- [C.block| void * {
    const char * seq = $bs-cstr:inp;
    vrna_fold_compound_t * c;
    vrna_md_t md;
    vrna_md_set_default (&md);
    c = vrna_fold_compound (seq, &md, 0);
    return c;
  } |]
  return c

-- | Destroy a given fold compound.

destroyFoldCompound :: Ptr () -> IO ()
destroyFoldCompound cptr = do
  [C.block| void {
    vrna_fold_compound_t * c = $(void * cptr);
    vrna_fold_compound_free (c);
  } |]

-- | Allow operating with a fold compound, hiding the creation.

withFoldCompound :: BS.ByteString -> (Ptr () -> a) -> a
withFoldCompound inp f = unsafePerformIO $ do
  c <- mkFoldCompound inp
  let !x = f c
  destroyFoldCompound c
  return x

-- | Calculate the energy and structure of the given input sequence.

mfe :: BS.ByteString -> (Double, BS.ByteString)
mfe inp = unsafePerformIO $ do
  c <- mkFoldCompound inp
  out <- BI.create (BS.length inp + 1) (\_ -> return ())
  e <- [C.block| float {
    vrna_fold_compound_t * c = $(void *c);
    vrna_mfe (c, $bs-cstr:out);
  } |]
  destroyFoldCompound c
  return (realToFrac e, out)

-- | Calculate the energy of the sequence, assuming that it forms a hairpin.

hairpin :: BS.ByteString -> Int
hairpin inp = hairpinP inp 0 (fromIntegral $ BS.length inp -1)

-- | Given some sequence, calculate the hairpin energy for the left and right position.

hairpinP :: BS.ByteString -> Int -> Int -> Int
hairpinP inp i j = unsafePerformIO $ do
  c <- mkFoldCompound inp
  let !e = hairpinCP c i j
  destroyFoldCompound c
  return e

-- | Low-level function that assumes a fold compound and returns the hairpin energy between the two
-- indices.

hairpinCP :: Ptr () -> Int -> Int -> Int
hairpinCP c (fromIntegral -> i) (fromIntegral -> j) = unsafePerformIO $ do
  e <- [C.block| int {
    vrna_fold_compound_t * c = $(void *c);
    vrna_eval_hp_loop (c, $(int i) , $(int j));
  } |]
  return $ fromIntegral e

test_interior_loops :: BS.ByteString -> Int -> Int -> Int -> Int -> Int
test_interior_loops inp (fromIntegral -> i) (fromIntegral -> j) (fromIntegral -> k) (fromIntegral -> l) = unsafePerformIO $ do
  cp <- mkFoldCompound inp
  e <- [C.block| int {
    vrna_fold_compound_t * c = $(void *cp);
    vrna_eval_int_loop(c, $(int i), $(int j), $(int k), $(int l));
  } |]
  destroyFoldCompound cp
  return $ fromIntegral e

