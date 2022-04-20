{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RiskManager where

import DDD
import Machines

-- base
import Data.Semigroup (Last(Last))
import Data.String (IsString)

-- QuickCheck
import Test.QuickCheck

-- text
import Data.Text (Text)

newtype Name = Name Text
  deriving stock Show
  deriving newtype IsString

newtype Surname = Surname Text
  deriving stock Show
  deriving newtype IsString

newtype TaxCode = TaxCode Text
  deriving stock Show
  deriving newtype IsString

data UserData = UserData
  { name :: Name
  , surname :: Surname
  , taxCode :: TaxCode
  }
  deriving stock Show

newtype Amount = EuroCents Int
  deriving stock Show
  deriving Arbitrary via (NonNegative Int)

newtype InstalmentsNumber = InstalmentsNumber Int
  deriving stock Show

data LoanDetails = LoanDetails
  { amount      :: Amount
  , instalments :: InstalmentsNumber
  }
  deriving stock Show

newtype MissedPaymentDeadlines = MissedPaymentDeadlines Int
  deriving stock Show
  deriving Arbitrary via (NonNegative Int)

data CreditBureauData = CreditBureauData
  { missedPaymentDeadlines :: MissedPaymentDeadlines
  , arrears                :: Amount
  }
  deriving stock Show

instance Arbitrary CreditBureauData where
  arbitrary = CreditBureauData <$> arbitrary <*> arbitrary

data RiskCommand
  = RegisterUsedData UserData
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
    action :: RiskState -> RiskCommand -> ([RiskEvent], RiskState)
    action NoData                                 (RegisterUsedData ud)          = ([UserDataRegistered ud], CollectedUserData ud)
    action NoData                                 (ProvideLoanDetails ld)        = ([], NoData)
    action NoData                                 (ProvideCreditBureauData cbd)  = ([], NoData)
    action (CollectedUserData ud)                 (RegisterUsedData ud')         = ([UserDataRegistered ud'], CollectedUserData ud')
    action (CollectedUserData ud)                 (ProvideLoanDetails ld)        = ([LoanDetailsProvided ld], CollectedLoanDetailsFirst ud ld)
    action (CollectedUserData ud)                 (ProvideCreditBureauData cbd)  = ([CreditBureauDataReceived cbd], ReceivedCreditBureauDataFirst ud cbd)
    action (CollectedLoanDetailsFirst ud ld)      (RegisterUsedData ud')         = ([UserDataRegistered ud'], CollectedLoanDetailsFirst ud' ld)
    action (CollectedLoanDetailsFirst ud ld)      (ProvideLoanDetails ld')       = ([LoanDetailsProvided ld'], CollectedLoanDetailsFirst ud ld')
    action (CollectedLoanDetailsFirst ud ld)      (ProvideCreditBureauData cbd)  = ([CreditBureauDataReceived cbd], CollectedAllData ud ld cbd)
    action (ReceivedCreditBureauDataFirst ud cbd) (RegisterUsedData ud')         = ([UserDataRegistered ud'], ReceivedCreditBureauDataFirst ud' cbd)
    action (ReceivedCreditBureauDataFirst ud cbd) (ProvideLoanDetails ld)        = ([LoanDetailsProvided ld], CollectedAllData ud ld cbd)
    action (ReceivedCreditBureauDataFirst ud cbd) (ProvideCreditBureauData cbd') = ([CreditBureauDataReceived cbd'], ReceivedCreditBureauDataFirst ud cbd')
    action (CollectedAllData ud ld cbd)           (RegisterUsedData ud')         = ([UserDataRegistered ud'], CollectedAllData ud' ld cbd)
    action (CollectedAllData ud ld cbd)           (ProvideLoanDetails ld')       = ([LoanDetailsProvided ld'], CollectedAllData ud ld' cbd)
    action (CollectedAllData ud ld cbd)           (ProvideCreditBureauData cbd') = ([CreditBureauDataReceived cbd'], CollectedAllData ud ld cbd')

    initialState :: RiskState
    initialState = NoData

interactWithCreditBureau :: UserData -> IO CreditBureauData
interactWithCreditBureau _ = generate arbitrary

riskPolicy :: Policy IO RiskEvent RiskCommand
riskPolicy = Policy $ statelessT action
  where
    action :: RiskEvent -> IO [RiskCommand]
    action (UserDataRegistered ud)        = pure . ProvideCreditBureauData <$> interactWithCreditBureau ud
    action (LoanDetailsProvided ld)       = pure []
    action (CreditBureauDataReceived cbd) = pure []

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
    action :: ReceivedData -> RiskEvent -> ReceivedData
    action receivedData (UserDataRegistered ud)        = receivedData { userData = Just ud }
    action receivedData (LoanDetailsProvided ld)       = receivedData { loanDetails = Just ld}
    action receivedData (CreditBureauDataReceived cbd) = receivedData { creditBureauData = Just cbd }

    initialState :: ReceivedData
    initialState = ReceivedData
      { userData         = Nothing
      , loanDetails      = Nothing
      , creditBureauData = Nothing
      }

riskManagerApplication :: Application IO RiskCommand RiskEvent ReceivedData
riskManagerApplication = Application riskAggregate (Just riskPolicy) riskProjection
