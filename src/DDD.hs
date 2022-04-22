{-# LANGUAGE Rank2Types #-}

module DDD where

import Machines (MealyT, Mealy, run, compose, feedback)

newtype Aggregate command event = Aggregate
  { aggregateMachine :: Mealy command [event] }

newtype Policy m event command = Policy
  { policyMachine :: MealyT m event [command] }

newtype Projection event readModel = Projection
  { projectionMachine :: Mealy event readModel }

data Application m command event readModel = Application
  { aggregate  :: Aggregate command event
  , policy     :: Maybe (Policy m event command)
  , projection :: Projection event readModel
  }

runApplication :: (Monad m, Foldable t, Monoid readModel) => Application m command event readModel -> t command -> m readModel
runApplication application commands = fst <$> run machine mempty commands
  where
    machine = compose
      mempty
      ((\p -> projectionMachine p) . projection $ application)
      (case policy application of
        Nothing      -> (\a -> aggregateMachine a) . aggregate $ application
        Just policy' -> feedback ((\a -> aggregateMachine a) . aggregate $ application) (policyMachine policy'))
