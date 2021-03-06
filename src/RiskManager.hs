{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module RiskManager where

import DDD
import Machines
import RiskManager.Types

-- base
import Data.Semigroup (Last(Last))

-- QuickCheck
import Test.QuickCheck

data RiskCommand
  = RegisterUserData UserData
  | ProvideLoanDetails LoanDetails
  | ProvideCreditBureauData CreditBureauData

data RiskEvent
  = UserDataRegistered UserData
  | LoanDetailsProvided LoanDetails
  | CreditBureauDataReceived CreditBureauData

data RiskState
  = NoData
  | CollectedUserData UserData
  | CollectedLoanDetailsFirst UserData LoanDetails
  | ReceivedCreditBureauDataFirst UserData CreditBureauData
  | CollectedAllData UserData LoanDetails CreditBureauData

riskAggregate :: Aggregate RiskCommand RiskEvent
riskAggregate = Aggregate $ mealy action initialState
  where
    -- a list is declared with `[_]`, the empty list is `[]`
    action :: RiskState -> RiskCommand -> ([RiskEvent], RiskState)
    action NoData                                 (RegisterUserData ud)            = (_, _)
    action NoData                                 (ProvideLoanDetails ld)          = (_, _)
    action NoData                                 (ProvideCreditBureauData cbd)    = (_, _)
    action (CollectedUserData ud)                 (RegisterUserData newUd)         = (_, _)
    action (CollectedUserData ud)                 (ProvideLoanDetails ld)          = (_, _)
    action (CollectedUserData ud)                 (ProvideCreditBureauData cbd)    = (_, _)
    action (CollectedLoanDetailsFirst ud ld)      (RegisterUserData newUd)         = (_, _)
    action (CollectedLoanDetailsFirst ud ld)      (ProvideLoanDetails newLd)       = (_, _)
    action (CollectedLoanDetailsFirst ud ld)      (ProvideCreditBureauData cbd)    = (_, _)
    action (ReceivedCreditBureauDataFirst ud cbd) (RegisterUserData newUd)         = (_, _)
    action (ReceivedCreditBureauDataFirst ud cbd) (ProvideLoanDetails ld)          = (_, _)
    action (ReceivedCreditBureauDataFirst ud cbd) (ProvideCreditBureauData newCbd) = (_, _)
    action (CollectedAllData ud ld cbd)           (RegisterUserData newUd)         = (_, _)
    action (CollectedAllData ud ld cbd)           (ProvideLoanDetails newLd)       = (_, _)
    action (CollectedAllData ud ld cbd)           (ProvideCreditBureauData newCbd) = (_, _)

    initialState :: RiskState
    initialState = _

data ReceivedData = ReceivedData
  { userData         :: Maybe UserData
  , loanDetails      :: Maybe LoanDetails
  , creditBureauData :: Maybe CreditBureauData
  }
  deriving stock Show
  deriving (Semigroup) via (Last ReceivedData)

instance Monoid ReceivedData where
  mempty = ReceivedData
    { userData         = Nothing
    , loanDetails      = Nothing
    , creditBureauData = Nothing
    }

riskProjection :: Projection RiskEvent ReceivedData
riskProjection = Projection $ stateful action initialState
  where
    -- you can update a record with the `record {fieldName = value}` syntax
    -- use the `Just` constructor to say that a `Maybe` actually contains data
    action :: ReceivedData -> RiskEvent -> ReceivedData
    action receivedData (UserDataRegistered ud)        = _
    action receivedData (LoanDetailsProvided ld)       = _
    action receivedData (CreditBureauDataReceived cbd) = _

    initialState :: ReceivedData
    initialState = _

interactWithCreditBureau :: UserData -> IO CreditBureauData
interactWithCreditBureau _ = generate arbitrary

riskPolicy :: Policy IO RiskEvent RiskCommand
riskPolicy = Policy $ statelessT action
  where
    -- use `_ <$> something` to map over a value `something :: IO a`
    -- use `pure` to return a value which doesn't to any outside world interaction
    action :: RiskEvent -> IO [RiskCommand]
    action (UserDataRegistered ud)        = _
    action (LoanDetailsProvided ld)       = _
    action (CreditBureauDataReceived cbd) = _

newtype UserDataUpdatesCount = UserDataUpdatesCount Int
  deriving (Eq, Show)
  deriving Semigroup via (Last Int)

instance Monoid UserDataUpdatesCount where
  mempty = UserDataUpdatesCount 0

userDataUpdatesCounter :: Projection RiskEvent UserDataUpdatesCount
userDataUpdatesCounter = Projection $ stateful action initialState
  where
    action :: UserDataUpdatesCount -> RiskEvent -> UserDataUpdatesCount
    action (UserDataUpdatesCount i) (UserDataRegistered _) = UserDataUpdatesCount (i + 1)
    action i _                                             = i

    initialState :: UserDataUpdatesCount
    initialState = UserDataUpdatesCount 0

riskManagerApplication :: Application IO RiskCommand RiskEvent ReceivedData
riskManagerApplication = Application riskAggregate (Just riskPolicy) riskProjection
