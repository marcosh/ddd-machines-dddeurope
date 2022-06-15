{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}

module DDD where

import Machines (MealyT, Mealy, run, compose, feedback)

-- base
import qualified Control.Category as Cat (Category (id, (.)))

-- profunctors
import Data.Profunctor

newtype Aggregate command event = Aggregate
  { aggregateMachine :: Mealy command [event] }

instance Profunctor Aggregate where
  lmap :: (a -> b) -> Aggregate b c -> Aggregate a c
  lmap f (Aggregate mealy) = Aggregate (lmap f mealy)

  rmap :: (b -> c) -> Aggregate a b -> Aggregate a c
  rmap f (Aggregate mealy) = Aggregate (rmap (fmap f) mealy)

instance Strong Aggregate where
  first' :: Aggregate a b -> Aggregate (a, c) (b, c)
  first' (Aggregate mealy) = Aggregate (rmap (\(bs, c) -> (, c) <$> bs) $ first' mealy)

newtype Policy m event command = Policy
  { policyMachine :: MealyT m event [command] }

instance Monad m => Cat.Category (Policy m) where
  id :: Policy m a a
  id = Policy (rmap pure Cat.id)

  (.) :: Policy m b c -> Policy m a b -> Policy m a c
  (.) (Policy mealy1) (Policy mealy2) = Policy (compose [] mealy1 mealy2)

instance Functor m => Profunctor (Policy m) where
  lmap :: (a -> b) -> Policy m b c -> Policy m a c
  lmap f (Policy mealy) = Policy (lmap f mealy)

  rmap :: (b -> c) -> Policy m a b -> Policy m a c
  rmap f (Policy mealy) = Policy (rmap (fmap f) mealy)

instance Functor m => Strong (Policy m) where
  first' :: Policy m a b -> Policy m (a, c) (b, c)
  first' (Policy mealy) = Policy (rmap (\(bs, c) -> (, c) <$> bs) $ first' mealy)

newtype Projection event readModel = Projection
  { projectionMachine :: Mealy event readModel }

instance Cat.Category Projection where
  id :: Projection a a
  id = Projection Cat.id

  (.) :: Projection b c -> Projection a b -> Projection a c
  (.) (Projection mealy1) (Projection mealy2) = Projection (mealy1 Cat.. mealy2)

instance Profunctor Projection where
  lmap :: (a -> b) -> Projection b c -> Projection a c
  lmap f (Projection mealy) = Projection (lmap f mealy)

  rmap :: (b -> c) -> Projection a b -> Projection a c
  rmap f (Projection mealy) = Projection (rmap f mealy)

instance Strong Projection where
  first' :: Projection a b -> Projection (a, c) (b, c)
  first' (Projection mealy) = Projection (first' mealy)

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
