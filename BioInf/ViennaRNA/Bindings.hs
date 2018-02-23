
-- | Bindings to important functions in the ViennaRNA library.
--
-- TODO Anything here that is not thread-safe should internally use
-- a mutex!

module BioInf.ViennaRNA.Bindings
  ( module BioInf.ViennaRNA.Bindings
  , CofoldF (..)
  , Duplex (..)
  , RNAfoldOptions (..)
  ) where

import           Data.ByteString.Char8
import qualified Data.Array.IArray as A
import qualified Data.Array.Unboxed as AU
import qualified Data.Array.Storable as AS

import           BioInf.ViennaRNA.Bindings.FFI.Centroid as FFI
import           BioInf.ViennaRNA.Bindings.FFI.CoFold   as FFI
import           BioInf.ViennaRNA.Bindings.FFI.Duplex   as FFI
import           BioInf.ViennaRNA.Bindings.FFI.Fold     as FFI
import           BioInf.ViennaRNA.Bindings.FFI.PartFunc as FFI
import           BioInf.ViennaRNA.Bindings.FFI.RNAfold as FFI

import           BioInf.ViennaRNA.Bindings.FFI.Utils as FFI

-- * new, generic bindings

-- | Interface to @RNAfold@. The 'RNAfoldOptions' allow settings which variants
-- of the folding to calculate. They allow settings additional options and
-- temperature, too.
--
-- The function returns a triple of (@Maybe@'s!) of @(mfe,ensemble,centroid@.
--
-- The @mfe@ yields the @minimum free energy, structure@ components.
--
-- The @ensemble@ yields the @ensemble gibbs free energy@, the @ensemble
-- structure@ (not a canonical secondary structure!), and the @array of pair
-- probabilities@.
--
-- The @centroid@ yields the @centroid free energy@, the @centroid structure@,
-- and @average centroid distance to all structures@.

rnafold
  :: RNAfoldOptions
  -> ByteString
  -> IO ( Maybe (Double, ByteString)
        , Maybe (Double, ByteString, AU.UArray (Int,Int) Double)
        , Maybe (Double, ByteString, Double)
        )
rnafold = ffi_RNAfold

-- * old bindings

-- | Fold a sequence into an optimal secondary structure. Returns a pair of
-- energy and structure.

mfe :: ByteString -> IO (Double,ByteString)
mfe = ffiFold

mfeTemp :: Double -> ByteString -> IO (Double,ByteString)
mfeTemp = ffiFoldTemp

circmfe :: ByteString -> IO (Double,ByteString)
circmfe = ffiCircFold

-- | Given a sequence and a structure, returns the energy of the
-- sequence/structure pair.

eos :: ByteString -> ByteString -> IO Double
eos i s = ffiEnergyOfStructure i s 0

eosTemp :: Double -> ByteString -> ByteString -> IO Double
eosTemp t i s = ffiEnergyOfStructureTemp t i s 0

-- | Energy of a circular structure

eosCirc :: ByteString -> ByteString -> IO Double
eosCirc i s = ffiEnergyOfCircStructure i s 0

-- | Given a string, calculates the partition function for said string. Returns
-- the ensemble energy, a string with where each nucleotide position is
-- annotated with the strength of the potential pairing, and the whole base
-- pair probability table.

part :: ByteString -> IO (Double,ByteString,A.Array (Int,Int) Double)
part = ffi_pf_fold

partConstrained :: ByteString -> ByteString -> IO (Double, ByteString, A.Array (Int,Int) Double)
partConstrained = ffi_pf_fold_constrained

circPart :: ByteString -> IO (Double,ByteString,A.Array (Int,Int) Double)
circPart = ffi_pf_circ_fold

circPartConstrained :: ByteString -> ByteString -> IO (Double, ByteString, A.Array (Int,Int) Double)
circPartConstrained = ffi_pf_circ_fold_constrained

-- | Centroid structure

centroidTemp :: Double -> ByteString -> IO (Double,ByteString,Double)
centroidTemp t i = ffiCentroidTemp t i

-- * RNAcofold

-- | Energy of struct for cofolded structures.

coeos :: ByteString -> ByteString -> Int -> IO Double
coeos i s c = ffiCoEnergyOfStructure c i s 0

-- | mfe of co-folded structure

comfe :: ByteString -> Int -> IO (Double,ByteString)
comfe s c = ffiCoFold c s

-- | Cofolded partition function. Makes the set of different partfun values
-- from cofoldF available.

copart :: ByteString -> Int -> IO (CofoldF,ByteString,A.Array (Int,Int) Double)
copart s c = ffiCoPartitionFunction  c s

copartConstrained :: ByteString -> ByteString -> Int -> IO (CofoldF,ByteString,A.Array (Int,Int) Double)
copartConstrained sq str c = ffiCoPartitionConstrained c sq str

-- | Fold a duplex structure

duplexFold :: ByteString -> ByteString -> IO Duplex
duplexFold = ffiDuplexFold

