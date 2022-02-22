{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Door where

import DDD (Aggregate (Aggregate), Projection (Projection), Policy (Policy), Application (Application))
import Machines (mealy, stateful, stateless)

-- base
import Data.Semigroup ( Last(Last) )

data DoorCommand = Knock | Open | Close
  deriving (Eq, Show)

data DoorState = IsOpen | IsClosed

data DoorEvent = Knocked | Opened | Closed
  deriving (Eq, Show)

doorAggregate :: Aggregate DoorCommand DoorEvent
doorAggregate = Aggregate $ mealy action initialState
  where
    action :: DoorState -> DoorCommand -> ([DoorEvent], DoorState)
    action IsOpen   Knock = ([Knocked], IsOpen  )
    action IsOpen   Open  = ([]       , IsOpen  )
    action IsOpen   Close = ([Closed] , IsClosed)
    action IsClosed Knock = ([Knocked], IsClosed)
    action IsClosed Open  = ([Opened] , IsOpen  )
    action IsClosed Close = ([]       , IsClosed)

    initialState :: DoorState
    initialState = IsClosed

newtype HowManyTimesTheDoorWasOpened = HowManyTimesTheDoorWasOpened Int
  deriving (Eq, Show, Num)
  deriving Semigroup via (Last Int)

instance Monoid HowManyTimesTheDoorWasOpened where
  mempty = 0

countHowManyTimesTheDoorWasOpened :: Projection DoorEvent HowManyTimesTheDoorWasOpened
countHowManyTimesTheDoorWasOpened = Projection $ stateful action initialState
  where
    action :: HowManyTimesTheDoorWasOpened -> DoorEvent -> HowManyTimesTheDoorWasOpened
    action n Opened = n + 1
    action n _      = n

    initialState = 0

doorOpensOnKnock :: Monad m => Policy m DoorEvent DoorCommand
doorOpensOnKnock = Policy $ stateless shouldOpenOnKnocked
  where
    shouldOpenOnKnocked :: DoorEvent -> [DoorCommand]
    shouldOpenOnKnocked Knocked = [Open]
    shouldOpenOnKnocked _       = []

doorApplication :: Monad m => Application m DoorCommand DoorEvent HowManyTimesTheDoorWasOpened
doorApplication = Application doorAggregate (Just doorOpensOnKnock) countHowManyTimesTheDoorWasOpened
