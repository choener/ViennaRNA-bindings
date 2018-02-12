{-# Language OverloadedStrings #-}

-- |
--
-- NOTE Remember that all calculations are done via @RNAfold -p --noLP@.

module Main where

import Control.Parallel.Strategies (parMap,rdeepseq)
import Data.Array ((!))
import Data.ByteString.Char8 ()
import Debug.Trace
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Silver as S
import Test.Tasty.Silver.Interactive as SI
import Test.Tasty.TH

import Biobase.Types.Sequence (RNAseq(..))
import BioInf.ViennaRNA.Bindings as V



a =~ b = abs (b - a) <= 0.01



case_mfe_001 :: Assertion
case_mfe_001 = do
  (e,s) <- V.mfe "cccaaaggg"
  assertBool "energy" $ e =~ (-1.2)
  assertBool "structure" $ s == "(((...)))"

case_mfe_002 :: Assertion
case_mfe_002 = do
  (e,s) <- V.mfe "uguagcuagcuagcuagcuacguacguagcuagc"
  assertBool "energy" $ e =~ (-14.0)
  assertBool "structure" $ s == "............(((((((((....)))))))))"

-- RNAfold webserver test sequence, allowing for isolated base pairs
--
-- TODO Currently set no have no lonely pairs. Soon, we should have a structure
-- with options to set with.

case_mfe_003 :: Assertion
case_mfe_003 = do
  (e,s) <- V.mfe "GGGCUAUUAGCUCAGUUGGUUAGAGCGCACCCCUGAUAAGGGUGAGGUCGCUGAUUCGAAUUCAGCAUAGCCCA"
  assertBool "energy" $ e =~ (-28.90)
  assertBool "structure" $ s == "(((((((..((((.........)))).(((((.......))))).....(((((.......))))))))))))."

--case_circmfe_001 :: Assertion
--case_circmfe_001 = do
--  (e,s) <- V.circmfe "GGGCUAUUAGCUCAGUUGGUUAGAGCGCA&CCCCUGAUAAGGGUGAGGUCGCUGAUUCGAAUUCAGCAUAGCCCA"
--  assertBool "energy" $ e =~ (-19.50)
--  assertBool "structure" $ s == ".((((((..(((.((((.(....(((((.(((((....)))).)..).))))....).)))).))))))))).."

case_eos_001 :: Assertion
case_eos_001 = do
  e <- V.eos "ACGAUCAGAGAUCAGAGCAUACGACAGCAG" "..((((...))))...((........)).."
  assertBool "eos" $ e =~ (-2.90)

case_eosTemp_37_001 :: Assertion
case_eosTemp_37_001 = do
  e <- V.eosTemp 37 "ACGAUCAGAGAUCAGAGCAUACGACAGCAG" "..((((...))))...((........)).."
  assertBool "eos" $ e =~ (-2.90)

case_eosTemp_20_001 :: Assertion
case_eosTemp_20_001 = do
  e <- V.eosTemp 20 "GGGCUAUUAGCUCAGUUGGUUAGAGCGCACCCCUGAUAAGGGUGAGGUCGCUGAUUCGAAUUCAGCAUAGCCCA" "((((((((((((.....)))))((.(.(((((.......))))).).))(((((.......))))))))))))."
  assertBool "eos" $ e =~ (-40.86)

-- TODO more on @arr@ checks !

case_part_001 :: Assertion
case_part_001 = do
  (e,s,arr) <- V.part "GGGCUAUUAGCUCAGUUGGUUAGAGCGCACCCCUGAUAAGGGUGAGGUCGCUGAUUCGAAUUCAGCAUAGCCCA"
  assertBool "energy" $ e =~ (-31.43)
  assertBool "structure" $ s == "(((((((..(((.{{{{,|,,.,({({((((({(....})))}).,,,)|||,,..}.}}}),))))))))))."
  assertBool "1,13" $ arr ! (1,13) =~ 0.010
  assertBool "4,70" $ arr ! (4,70) =~ 0.999

case_duplexfold_001 :: Assertion
case_duplexfold_001 = do
  d <- V.duplexFold "ACGATCAGAGATCAGAGCATACGACAGCAG" "ACGAAAAAAAGAGCATACGACAGCAG"
  assertBool "energy" $ energy d =~ (-4.10)
  assertBool "structure" $ structure d == ".((...((...((.&.))...))...))."

case_mfeTemp_37_001 :: Assertion
case_mfeTemp_37_001 = do
  (e,s) <- V.mfeTemp 37 "cccaaaggg"
  assertBool "energy" $ e =~ (-1.2)
  assertBool "structure" $ s == "(((...)))"

case_mfeTemp_37_002 :: Assertion
case_mfeTemp_37_002 = do
  (e,s) <- V.mfeTemp 37 "uguagcuagcuagcuagcuacguacguagcuagc"
  assertBool "energy" $ e =~ (-14.0)
  assertBool "structure" $ s == "............(((((((((....)))))))))"

case_mfeTemp_37_003 :: Assertion
case_mfeTemp_37_003 = do
  (e,s) <- V.mfeTemp 37 "GGGCUAUUAGCUCAGUUGGUUAGAGCGCACCCCUGAUAAGGGUGAGGUCGCUGAUUCGAAUUCAGCAUAGCCCA"
  assertBool "energy" $ e =~ (-28.90)
  assertBool "structure" $ s == "(((((((..((((.........)))).(((((.......))))).....(((((.......))))))))))))."

case_mfeTemp_20_001 :: Assertion
case_mfeTemp_20_001 = do
  (e,s) <- V.mfeTemp 20 "GGGCUAUUAGCUCAGUUGGUUAGAGCGCACCCCUGAUAAGGGUGAGGUCGCUGAUUCGAAUUCAGCAUAGCCCA"
  assertBool "energy" $ e =~ (-39.29)
  assertBool "structure" $ s == "(((((((..((((.........)))).(((((.......))))).....(((((.......))))))))))))."



case_centroidTemp_37_001 :: Assertion
case_centroidTemp_37_001 = do
  (e,s,d) <- V.centroidTemp 37 "cccaaaggg"
  assertBool "energy" $ e =~ (-1.2)
  assertBool "structure" $ s == "(((...)))"
  assertBool "avg distance" $ d =~ 1.06

case_centroidTemp_37_003 :: Assertion
case_centroidTemp_37_003 = do
  (e,s,d) <- V.centroidTemp 37 "GGGCUAUUAGCUCAGUUGGUUAGAGCGCACCCCUGAUAAGGGUGAGGUCGCUGAUUCGAAUUCAGCAUAGCCCA"
  assertBool "energy" $ e =~ (-28.10)
  assertBool "structure" $ s == "(((((((..((((.........)))).(((((.(....)))))).....(((((.......))))))))))))."
  -- using @--noLP@
  assertBool "avg distance" $ d =~ 11.86

-- | Parallelization test.

prop_parallel_mfeTemp ∷ [RNAseq] → Bool
prop_parallel_mfeTemp ss = ns == ps
  where ns = map f ss
        ps = parMap rdeepseq f ss
        f (RNAseq s) = unsafePerformIO $ V.mfeTemp 37 s



main :: IO ()
main = $(defaultMainGenerator)

