
-- | Bindings to important functions in the ViennaRNA library.

module BioInf.ViennaRNA.Bindings where

import qualified Data.Array.IArray as A

import BioInf.ViennaRNA.Bindings.FFI.Fold as FFI
import BioInf.ViennaRNA.Bindings.FFI.PartFunc as FFI

-- | Fold a sequence into an optimal secondary structure. Returns a pair of
-- energy and structure.

fold :: String -> IO (Double,String)
fold = ffiFold

-- | Given a sequence and a structure, returns the energy of the
-- sequence/structure pair.

eos :: String -> String -> IO Double
eos i s = ffiEnergyOfStructure i s 0

-- | Given a string, calculates the partition function for said string. Returns
-- the ensemble energy, a string with where each nucleotide position is
-- annotated with the strength of the potential pairing, and the whole base
-- pair probability table.

part :: String -> IO (Double,String,A.Array (Int,Int) Double)
part = ffiPartitionFunction

