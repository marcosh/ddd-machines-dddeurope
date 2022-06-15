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
    action NoData                                 (RegisterUserData ud)            = ([UserDataRegistered ud], CollectedUserData ud)
    action NoData                                 (ProvideLoanDetails ld)          = ([], NoData)
    action NoData                                 (ProvideCreditBureauData cbd)    = ([], NoData)
    action (CollectedUserData ud)                 (RegisterUserData newUd)         = ([UserDataRegistered newUd], CollectedUserData newUd)
    action (CollectedUserData ud)                 (ProvideLoanDetails ld)          = ([LoanDetailsProvided ld], CollectedLoanDetailsFirst ud ld)
    action (CollectedUserData ud)                 (ProvideCreditBureauData cbd)    = ([CreditBureauDataReceived cbd], ReceivedCreditBureauDataFirst ud cbd)
    action (CollectedLoanDetailsFirst ud ld)      (RegisterUserData newUd)         = ([UserDataRegistered newUd], CollectedLoanDetailsFirst newUd ld)
    action (CollectedLoanDetailsFirst ud ld)      (ProvideLoanDetails newLd)       = ([LoanDetailsProvided newLd], CollectedLoanDetailsFirst ud newLd)
    action (CollectedLoanDetailsFirst ud ld)      (ProvideCreditBureauData cbd)    = ([CreditBureauDataReceived cbd], CollectedAllData ud ld cbd)
    action (ReceivedCreditBureauDataFirst ud cbd) (RegisterUserData newUd)         = ([UserDataRegistered newUd], ReceivedCreditBureauDataFirst newUd cbd)
    action (ReceivedCreditBureauDataFirst ud cbd) (ProvideLoanDetails ld)          = ([LoanDetailsProvided ld], CollectedAllData ud ld cbd)
    action (ReceivedCreditBureauDataFirst ud cbd) (ProvideCreditBureauData newCbd) = ([CreditBureauDataReceived newCbd], ReceivedCreditBureauDataFirst ud newCbd)
    action (CollectedAllData ud ld cbd)           (RegisterUserData newUd)         = ([UserDataRegistered newUd], CollectedAllData newUd ld cbd)
    action (CollectedAllData ud ld cbd)           (ProvideLoanDetails newLd)       = ([LoanDetailsProvided newLd], CollectedAllData ud newLd cbd)
    action (CollectedAllData ud ld cbd)           (ProvideCreditBureauData newCbd) = ([CreditBureauDataReceived newCbd], CollectedAllData ud ld newCbd)

    initialState :: RiskState
    initialState = NoData

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
    action receivedData (UserDataRegistered ud)        = receivedData { userData = Just ud}
    action receivedData (LoanDetailsProvided ld)       = receivedData { loanDetails = Just ld}
    action receivedData (CreditBureauDataReceived cbd) = receivedData { creditBureauData = Just cbd}

    initialState :: ReceivedData
    initialState = mempty

interactWithCreditBureau :: UserData -> IO CreditBureauData
interactWithCreditBureau _ = generate arbitrary

riskPolicy :: Policy IO RiskEvent RiskCommand
riskPolicy = Policy $ statelessT action
  where
    -- use `_ <$> something` to map over a value `something :: IO a`
    -- use `pure` to return a value which doesn't to any outside world interaction
    action :: RiskEvent -> IO [RiskCommand]
    action (UserDataRegistered ud)        = (\cbd -> [ProvideCreditBureauData cbd]) <$> interactWithCreditBureau ud
    action (LoanDetailsProvided ld)       = pure []
    action (CreditBureauDataReceived cbd) = pure []

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
