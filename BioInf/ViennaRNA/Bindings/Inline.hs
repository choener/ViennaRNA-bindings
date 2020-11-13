{-# Language QuasiQuotes #-}
{-# Language TemplateHaskell #-}

module BioInf.ViennaRNA.Bindings.Inline where

import Data.ByteString.Char8 as BS
import qualified Language.C.Inline as C
import Foreign.C.Types
import Foreign.Ptr


C.context (C.baseCtx <> C.bsCtx)
C.include "../C/ViennaRNA/hairpin_loops.h"
C.include "../C/ViennaRNA/fold.h"

mkFoldCompound :: BS.ByteString -> IO (Ptr ())
mkFoldCompound inp = do
  c <- [C.block| void * {
    const char * seq = $bs-ptr:inp;
    vrna_fold_compound_t * c;
    vrna_md_t md;
    vrna_md_set_default (&md);
    c = vrna_fold_compound (seq, &md, 0);
    return c;
  } |]
  return c

destroyFoldCompound :: Ptr () -> IO ()
destroyFoldCompound cptr = do
  [C.block| void {
    vrna_fold_compound_t * c = $(void * cptr);
    vrna_fold_compound_free (c);
  } |]

mfe :: BS.ByteString -> IO C.CFloat
mfe inp = do
  c <- mkFoldCompound inp
  e <- [C.block| float {
    vrna_fold_compound_t * c = $(void *c);
    vrna_mfe (c, 0);
  } |]
  destroyFoldCompound c
  return e

hairpin :: BS.ByteString -> IO C.CInt
hairpin inp = hairpinP inp 0 (fromIntegral $ BS.length inp -1)

hairpinP :: BS.ByteString -> C.CInt -> C.CInt -> IO C.CInt
hairpinP inp i j = do
  c <- mkFoldCompound inp
  e <- hairpinCP c i j
  destroyFoldCompound c
  return e

hairpinCP :: Ptr () -> C.CInt -> C.CInt -> IO C.CInt
hairpinCP c i j = [C.block| int {
    vrna_fold_compound_t * c = $(void *c);
    vrna_eval_hp_loop (c, $(int i) , $(int j));
  } |]

