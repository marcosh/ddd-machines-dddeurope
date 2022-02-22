module DoorSpec where

import DDD (aggregateMachine, Projection (projectionMachine), Policy (policyMachine))
import Door (DoorCommand(..), DoorEvent(..), doorAggregate, countHowManyTimesTheDoorWasOpened, doorOpensOnKnock)
import Machines (run, runMealyT)

-- base
import Data.Functor.Identity (runIdentity)

-- hspec
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "Door" $ do
    describe "Aggregate" $ do
      it "emits a knocked event when someone knocks" $ do
        (fst . runIdentity $ runMealyT (aggregateMachine doorAggregate) Knock) `shouldBe` [Knocked]

      it "emits an opened event when someone opens the door" $ do
        (fst . runIdentity $ runMealyT (aggregateMachine doorAggregate) Open) `shouldBe` [Opened]

      it "does not emit events when someone closes the door" $ do
        (fst . runIdentity $ runMealyT (aggregateMachine doorAggregate) Close) `shouldBe` []

      it "emits an opened and a knocked events when someone opens a door and knowks" $ do
        (fst . runIdentity $ run (aggregateMachine doorAggregate) [] [Open, Knock]) `shouldBe` [Opened, Knocked]

      it "emits an opened event when someone opens a door twice" $ do
        (fst . runIdentity $ run (aggregateMachine doorAggregate) [] [Open, Open]) `shouldBe` [Opened]

      it "emits an opened and closed events when someone opens and closes a door" $ do
        (fst . runIdentity $ run (aggregateMachine doorAggregate) [] [Open, Close]) `shouldBe` [Opened, Closed]

    describe "Projection" $ do
      it "increments the counter if an Opened event is emitted" $ do
        (fst . runIdentity $ runMealyT (projectionMachine countHowManyTimesTheDoorWasOpened) Opened) `shouldBe` 1

      it "does not increment the counter if a Knocked event is emitted" $ do
        (fst . runIdentity $ runMealyT (projectionMachine countHowManyTimesTheDoorWasOpened) Knocked) `shouldBe` 0

      it "does not increment the counter if a Closed event is emitted" $ do
        (fst . runIdentity $ runMealyT (projectionMachine countHowManyTimesTheDoorWasOpened) Closed) `shouldBe` 0

      it "counts correctly the number of Opened events emitted" $ do
        (fst . runIdentity $ run (projectionMachine countHowManyTimesTheDoorWasOpened) 0 [Opened, Closed, Opened, Knocked, Opened]) `shouldBe` 3

    describe "Policy" $ do
      it "emits Open command on Knocked event" $ do
        (fst . runIdentity $ runMealyT (policyMachine doorOpensOnKnock) Knocked) `shouldBe` [Open]

      it "does not emits commands on Opened event" $ do
        (fst . runIdentity $ runMealyT (policyMachine doorOpensOnKnock) Opened) `shouldBe` []

      it "does not emits commands on Closed event" $ do
        (fst . runIdentity $ runMealyT (policyMachine doorOpensOnKnock) Closed) `shouldBe` []
      
