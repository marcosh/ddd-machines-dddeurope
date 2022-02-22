{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MachinesSpec where

import Machines (Mealy, MealyT(runMealyT), feedback, mealy, run, stateless)

-- base
import Data.Functor.Identity ( Identity(Identity) )

-- hspec
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

-- QuickCheck
import Test.QuickCheck (arbitrary, forAll)

echo :: Monad m => MealyT m a [a]
echo = stateless pure

triangular :: Mealy Int [Int]
triangular = mealy update 0
  where
    update :: Int -> Int -> ([Int], Int)
    update state input = ([state + input], state + 1)

discard :: Mealy Int [Int]
discard = mealy update ()
  where
    update :: () -> Int -> ([Int], ())
    update _ _ = ([], ())

plus1upTo4 :: Mealy Int [Int]
plus1upTo4 = mealy update ()
  where
    update :: () -> Int -> ([Int], ())
    update _ i =
      if   i < 5
      then ([i + 1], ())
      else ([], ())

addIndex :: Int -> [Int] -> [Int]
addIndex _ []     = []
addIndex i (x:xs) = i + x : addIndex (i + 1) xs

checkMealy :: (Monad m, Eq (m b), Show (m b), Semigroup b) => MealyT m a b -> b -> [a] -> b -> Expectation
checkMealy machine initial inputs output =
  (fst <$> run machine initial inputs) `shouldBe` pure output

spec :: Spec
spec =
  describe "MealyT" $ do
    describe "echo machine" $ do
      it "should return its input" $ do
        forAll arbitrary $
          \(s :: String) -> (fst <$> runMealyT echo s) `shouldBe` Identity [s]

    describe "run" $ do
      describe "echo machine" $ do
        it "should return the inputs" $ do
          forAll arbitrary $
            \(ss :: [String]) -> checkMealy @Identity echo [] ss ss

      describe "triangular machine" $ do
        it "with triangular machine should return stateful results" $ do
          forAll arbitrary $
            \(is :: [Int]) -> checkMealy @Identity triangular [] is (addIndex 0 is)

    describe "feedback" $ do
      it "should process multiple inputs" $ do
        checkMealy @Identity (feedback echo discard) [] [1, 1] [1, 1]

      it "should use the feedback process" $ do
        checkMealy @Identity (feedback echo plus1upTo4) [] [1] [1, 2, 3, 4, 5]
        checkMealy @Identity (feedback echo plus1upTo4) [] [2] [2, 3, 4, 5]
        checkMealy @Identity (feedback echo plus1upTo4) [] [3] [3, 4, 5]
        checkMealy @Identity (feedback echo plus1upTo4) [] [4] [4, 5]
        checkMealy @Identity (feedback echo plus1upTo4) [] [5] [5]
        checkMealy @Identity (feedback echo plus1upTo4) [] [6] [6]
        checkMealy @Identity (feedback echo plus1upTo4) [] [1, 1] [1, 2, 3, 4, 5, 1, 2, 3, 4, 5]
        checkMealy @Identity (feedback echo plus1upTo4) [] [1, 2] [1, 2, 3, 4, 5, 2, 3, 4, 5]
        checkMealy @Identity (feedback echo plus1upTo4) [] [3, 5, 1] [3, 4, 5, 5, 1, 2, 3, 4, 5]

      it "should propagate the state" $ do
        (take 10 . fst <$> run (feedback triangular echo) [] [0]) `shouldBe` Identity [0, 1, 3, 6, 10, 15, 21, 28, 36, 45]
