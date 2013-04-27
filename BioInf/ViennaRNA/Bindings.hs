
-- | Bindings to everything.

module BioInf.ViennaRNA.Bindings where

import BioInf.ViennaRNA.Bindings.FFI.Fold as FFI
import BioInf.ViennaRNA.Bindings.FFI.PartFunc as FFI

fold = ffiFold
eos i s = ffiEnergyOfStructure i s 0
part = ffiPartitionFunction
