
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Silver as S
import Test.Tasty.Silver.Interactive as SI
import Test.Tasty.TH

import BioInf.ViennaRNA.Bindings as V



a =~ b = abs (b - a) <= 0.01



case_s_001 :: Assertion
case_s_001 = do
  (e,s) <- V.mfe "cccaaaggg"
  assertBool "energy" $ e =~ (-1.2)
  assertBool "structure" $ s == "(((...)))"

case_s_002 :: Assertion
case_s_002 = do
  (e,s) <- V.mfe "uguagcuagcuagcuagcuacguacguagcuagc"
  assertBool "energy" $ e =~ (-14.3)
  assertBool "structure" $ s == "............(((((((((....)))))))))"



main :: IO ()
main = $(defaultMainGenerator)

