
module BioInf.ViennaRNA.Bindings.Fold where

import BioInf.ViennaRNA.Bindings.FFI.Fold as FFI
import BioInf.ViennaRNA.Bindings.FFI.PartFunc as FFI

fold = ffiFold
eos i s = ffiEnergyOfStructure i s 0
