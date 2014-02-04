
-- | Bindings to important functions in the ViennaRNA library.

module BioInf.ViennaRNA.Bindings
  ( mfe
  , eos
  , part
  , coeos
  , comfe
  , copart
  , CofoldF (..)
  ) where

import qualified Data.Array.IArray as A

import BioInf.ViennaRNA.Bindings.FFI.CoFold   as FFI
import BioInf.ViennaRNA.Bindings.FFI.Fold     as FFI
import BioInf.ViennaRNA.Bindings.FFI.PartFunc as FFI

-- | Fold a sequence into an optimal secondary structure. Returns a pair of
-- energy and structure.

mfe :: String -> IO (Double,String)
mfe = ffiFold

-- | Given a sequence and a structure, returns the energy of the
-- sequence/structure pair.

eos :: String -> String -> IO Double
eos i s = ffiEnergyOfStructure i s 0

-- | Given a string, calculates the partition function for said string. Returns
-- the ensemble energy, a string with where each nucleotide position is
-- annotated with the strength of the potential pairing, and the whole base
-- pair probability table.

part :: String -> IO (Double,String,A.Array (Int,Int) Double)
part = ffi_pf_fold

partConstrained :: String -> String -> IO (Double, String, A.Array (Int,Int) Double)
partConstrained = ffi_pf_fold_constrained



-- * RNAcofold

-- | Energy of struct for cofolded structures.

coeos :: String -> String -> Int -> IO Double
coeos i s c = ffiCoEnergyOfStructure c i s 0

-- | mfe of co-folded structure

comfe :: String -> Int -> IO (Double,String)
comfe s c = ffiCoFold c s

-- | Cofolded partition function. Makes the set of different partfun values
-- from cofoldF available.

copart :: String -> Int -> IO (CofoldF,String,A.Array (Int,Int) Double)
copart s c = ffiCoPartitionFunction  c s

