
module Main where

import qualified Data.Array.IArray as A

import BioInf.ViennaRNA.Bindings

main = do
  fold "CCCAAAGGG" >>= print
  eos "CCCAAAGGG" "(((...)))" >>= print
  p <- part "CCCCAAAAGGGGA"
  print p
  let (_,_,p') = p in mapM_ print $ {- zip [(i,j) | i <- [13,12..1], j <- [13,12..i]] -} A.assocs (p')
--  part "agcugucaucgaucgaucgaucgaucgaucgaucgaugcuagcuacgaucgaucguagcuagcuagcuacguagcuagcuagcuacgaugcaucgaucgaucgaucgaugc" >>= print
