
module BioInf.ViennaRNA.Bindings.FFI.Duplex
  ( Duplex (..)
  , ffiDuplexFold
  ) where

import           Control.Applicative
import           Control.Monad
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Float
import qualified Data.Array.IArray as A
import           Unsafe.Coerce

import           BioInf.ViennaRNA.Bindings.FFI.Utils



#include "duplex.h"
#include "data_structures.h"

{#pointer *duplexT as DuplexPtr -> Duplex #}

data Duplex = Duplex
  { i                 :: {-# UNPACK #-} !Int
  , j                 :: {-# UNPACK #-} !Int
  , end               :: {-# UNPACK #-} !Int
  , structure         ::                !String
  , energy            :: {-# UNPACK #-} !Double
  , energyBacktrack   :: {-# UNPACK #-} !Double
  , openingBacktrackX :: {-# UNPACK #-} !Double
  , openingBacktrackY :: {-# UNPACK #-} !Double
  , offset            :: {-# UNPACK #-} !Int
  , dG1               :: {-# UNPACK #-} !Double
  , dG2               :: {-# UNPACK #-} !Double
  , ddG               :: {-# UNPACK #-} !Double
  , tb                :: {-# UNPACK #-} !Int
  , te                :: {-# UNPACK #-} !Int
  , qb                :: {-# UNPACK #-} !Int
  , qe                :: {-# UNPACK #-} !Int
  }
  deriving (Show)

instance Storable Duplex where
  sizeOf _ = {# sizeof duplexT #}
  alignment _ = sizeOf (undefined :: CDouble)
  peek p = Duplex
    <$> liftM fromIntegral ({# get duplexT->i #} p)
    <*> liftM fromIntegral ({# get duplexT->j #} p)
    <*> liftM fromIntegral ({# get duplexT->end #} p)
    <*> (peekCAString =<<  ({# get duplexT->structure #} p))
    <*> liftM realToFrac   ({# get duplexT->energy #} p)
    <*> liftM realToFrac   ({# get duplexT->energy_backtrack #} p)
    <*> liftM realToFrac   ({# get duplexT->opening_backtrack_x #} p)
    <*> liftM realToFrac   ({# get duplexT->opening_backtrack_y #} p)
    <*> liftM fromIntegral ({# get duplexT->offset #} p)
    <*> liftM realToFrac   ({# get duplexT->dG1 #} p)
    <*> liftM realToFrac   ({# get duplexT->dG2 #} p)
    <*> liftM realToFrac   ({# get duplexT->ddG #} p)
    <*> liftM fromIntegral ({# get duplexT->tb #} p)
    <*> liftM fromIntegral ({# get duplexT->te #} p)
    <*> liftM fromIntegral ({# get duplexT->qb #} p)
    <*> liftM fromIntegral ({# get duplexT->qe #} p)

ffiDuplexFold :: String -> String -> IO Duplex
ffiDuplexFold l r =
  withCAString l $ \cl  ->
  withCAString r $ \cr  ->
  alloca         $ \ptr -> do
  d <- duplexfold_p ptr cl cr >> peek ptr
  return d

foreign import ccall "ffiwrap_duplexfold" duplexfold_p :: DuplexPtr -> CString -> CString -> IO ()

