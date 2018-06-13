{-# Language OverloadedStrings #-}

-- |
--
-- NOTE Remember that all calculations are done via @RNAfold -p --noLP@.

module Main where

import Control.Monad (forM_)
import Control.DeepSeq
import Control.Parallel.Strategies (parMap,rdeepseq)
import Data.Array.IArray ((!))
import qualified Data.Array.IArray as AI
import Data.Array.Base (UArray(..))
import Data.ByteString.Char8 ()
import Debug.Trace
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Silver as S
import Test.Tasty.Silver.Interactive as SI
import Test.Tasty.TH

import Biobase.Types.NucleotideSequence (RNAseq(..))
import BioInf.ViennaRNA.Bindings as V



a =~ b = abs (b - a) <= 0.01
a =~~~ b = abs (b - a) <= 0.000001


rnao = RNAfoldOptions
  { _fomfe = True
  , _focentroid = True
  , _foensemble = True
  , _fotemperature = 37
  , _fodangles = 2
  , _fonogu = True
  , _fonolp = True
  }


case_mfe_001 :: Assertion
case_mfe_001 = do
  (Just (e,s),_,_) <- V.rnafold rnao "cccaaaggg"
  assertBool "energy" $ e =~ (-1.2)
  assertBool "structure" $ s == "(((...)))"

case_mfe_002 :: Assertion
case_mfe_002 = do
  (Just (e,s),_,_) <- V.rnafold rnao "uguagcuagcuagcuagcuacguacguagcuagc"
  assertBool "energy" $ e =~ (-14.0)
  assertBool "structure" $ s == "............(((((((((....)))))))))"

-- RNAfold webserver test sequence, allowing for isolated base pairs
--
-- TODO Currently set no have no lonely pairs. Soon, we should have a structure
-- with options to set with.

case_mfe_003 :: Assertion
case_mfe_003 = do
  (Just (e,s),_,_) <- V.rnafold rnao "GGGCUAUUAGCUCAGUUGGUUAGAGCGCACCCCUGAUAAGGGUGAGGUCGCUGAUUCGAAUUCAGCAUAGCCCA"
  assertBool "energy" $ e =~ (-28.90)
  assertBool "structure" $ s == "(((((((..((((.........)))).(((((.......))))).....(((((.......))))))))))))."

case_mfe_004 :: Assertion
case_mfe_004 = do
  (Just (e,s),_,_) <- V.rnafold rnao "uguagcuagcuagcuagcuacguacguagcuagc"
  assertBool "energy" $ e =~ (-14.0)
  assertBool "structure" $ s == "............(((((((((....)))))))))"

case_mfe_005 :: Assertion
case_mfe_005 = do
  (Just (e,s),_,_) <- V.rnafold rnao "GGGCUAUUAGCUCAGUUGGUUAGAGCGCACCCCUGAUAAGGGUGAGGUCGCUGAUUCGAAUUCAGCAUAGCCCA"
  assertBool "energy" $ e =~ (-28.90)
  assertBool "structure" $ s == "(((((((..((((.........)))).(((((.......))))).....(((((.......))))))))))))."

case_mfeTemp_20_001 :: Assertion
case_mfeTemp_20_001 = do
  (Just (e,s),_,_) <- V.rnafold rnao{_fotemperature = 20} "GGGCUAUUAGCUCAGUUGGUUAGAGCGCACCCCUGAUAAGGGUGAGGUCGCUGAUUCGAAUUCAGCAUAGCCCA"
  assertBool "energy" $ e =~ (-39.29)
  assertBool "structure" $ s == "(((((((..((((.........)))).(((((.......))))).....(((((.......))))))))))))."

-- TODO more on @arr@ checks !

case_part_001 :: Assertion
case_part_001 = do
  let o = rnao{_fonogu = False, _fonolp = True, _fodangles = 2}
  --(e,s,arr) <- V.part "GGGCUAUUAGCUCAGUUGGUUAGAGCGCACCCCUGAUAAGGGUGAGGUCGCUGAUUCGAAUUCAGCAUAGCCCA"
  ( Just (mfeE,mfeS2)
    , Just (ensembleE,ensembleS2,ensembleArr)
    , Just (centroidE,centroidS2,centroidDist)
    ) <- V.rnafold o "GGGCUAUUAGCUCAGUUGGUUAGAGCGCACCCCUGAUAAGGGUGAGGUCGCUGAUUCGAAUUCAGCAUAGCCCA"
--  mapM_ print $ [ (k,v) | (k,v) <- AI.assocs ensembleArr, v > 0.5 ] -- , k == (1,13) ]
  -- mfe
  assertBool "mfe energy" $ mfeE =~ (-28.90)
  assertBool "mfe structure" $ mfeS2 == "(((((((..((((.........)))).(((((.......))))).....(((((.......))))))))))))."
  -- ensemble
  assertBool "ensemble gibbs free energy" $ ensembleE =~ (-29.97)
  assertBool "ensemble structure" $ ensembleS2 == "(((((((..((({..,,.....}|||.(((((,{....})))))....,{||{{.......)}))))))))))."
  forM_
    [ ( 1, 73, 0.999200770)
    , ( 2, 72, 0.999953925)
    , ( 3, 71, 0.999961554)
    , ( 4, 70, 0.999954759)
    , ( 5, 69, 0.998632659)
    , ( 6, 68, 0.988347048)
    , ( 7, 67, 0.900660774)
    , (28, 44, 0.914199199)
    , (29, 43, 0.917305662)
    ] $ \(i,j,p') -> let p = p'^2 in assertBool (show (i,j,p,"/~=",ensembleArr ! (i,j))) $ ensembleArr ! (i,j) =~ p

  --assertBool "1,13" $ ensembleArr ! (1,13) =~~~ 0.006288612
  --assertBool "4,70" $ ensembleArr ! (4,70) =~~~ 0.999954759
  -- centroid
  assertBool "centroid energy" $ centroidE =~ (-28.10)
  assertBool "centroid structure" $ centroidS2 == "(((((((..((((.........)))).(((((.(....)))))).....(((((.......))))))))))))."
  assertBool "centroid distance" $ centroidDist =~ 11.86



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

case_duplexfold_001 :: Assertion
case_duplexfold_001 = do
  d <- V.duplexFold "ACGATCAGAGATCAGAGCATACGACAGCAG" "ACGAAAAAAAGAGCATACGACAGCAG"
  assertBool "energy" $ energy d =~ (-4.10)
  assertBool "structure" $ structure d == ".((...((...((.&.))...))...))."



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

-- | Parallelization test for full RNAfold with all options.

prop_parallel_RNAfold ∷ [RNAseq] → Bool
prop_parallel_RNAfold ss = ns == ps
  where ns = map f ss
        ps = parMap rdeepseq f ss
        f (RNAseq s) = unsafePerformIO $ V.rnafold rnao s
--        f (RNAseq s) = unsafePerformIO $ V.mfeTemp 37 s

-- orphaned instance for deepseq (that should be ok, it can't leak from this
-- executable)

instance NFData (UArray (Int,Int) Double) where
  rnf (UArray !l !h !k !arr) = ()


main :: IO ()
main = $(defaultMainGenerator)

