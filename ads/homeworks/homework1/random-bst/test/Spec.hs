{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving  #-}

import           Test.Framework                 ( defaultMain
                                                , testGroup
                                                )
import           Test.Framework.Providers.QuickCheck2
                                                ( testProperty )
import           Data.List
import           RandomBST
import           System.Random

import           Test.QuickCheck


newtype ListArrayTest = ListArrayTest { unList :: [Int] }
    deriving newtype Show

instance Arbitrary ListArrayTest where
  arbitrary =
    ListArrayTest
      <$>        (listOf $ choose (100, 2000) :: Gen [Int])
      `suchThat` isUnique
    where isUnique x = nub x == x

main :: IO ()
main = do
  gen <- newStdGen
  defaultMain $ tests gen

tests gen =
  [ testGroup
      "Insertion RandomBST with height in log_n time"
      [ testProperty "Insertion with random lists from 10 to 2000 elems cases"
                     (withMaxSuccess 1000 (insertionRandom gen))
      ]
  ]

generateLists :: Gen [Int]
generateLists = unList <$> (arbitrary :: Gen ListArrayTest) `suchThat` \l -> (not $ Data.List.null (unList l)) && (length $ (unList l)) > 20

insertionRandom :: StdGen -> Property
insertionRandom gen =
  forAll generateLists $ \xs ->
  let
    lengthList    = length xs
    expected =
      (logBase ((fromIntegral 2) :: Float) ((fromIntegral lengthList) :: Float))
        * 3
  in
    fromIntegral (height $ (fromList xs gen :: RTreap StdGen Int Int))
      <= expected
  where types = gen::StdGen

