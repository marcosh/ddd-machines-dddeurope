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
    action NoData                                 (RegisterUsedData ud)          = (_, _)
    action NoData                                 (ProvideLoanDetails ld)        = (_, _)
    action NoData                                 (ProvideCreditBureauData cbd)  = (_, _)
    action (CollectedUserData ud)                 (RegisterUsedData ud')         = (_, _)
    action (CollectedUserData ud)                 (ProvideLoanDetails ld)        = (_, _)
    action (CollectedUserData ud)                 (ProvideCreditBureauData cbd)  = (_, _)
    action (CollectedLoanDetailsFirst ud ld)      (RegisterUsedData ud')         = (_, _)
    action (CollectedLoanDetailsFirst ud ld)      (ProvideLoanDetails ld')       = (_, _)
    action (CollectedLoanDetailsFirst ud ld)      (ProvideCreditBureauData cbd)  = (_, _)
    action (ReceivedCreditBureauDataFirst ud cbd) (RegisterUsedData ud')         = (_, _)
    action (ReceivedCreditBureauDataFirst ud cbd) (ProvideLoanDetails ld)        = (_, _)
    action (ReceivedCreditBureauDataFirst ud cbd) (ProvideCreditBureauData cbd') = (_, _)
    action (CollectedAllData ud ld cbd)           (RegisterUsedData ud')         = (_, _)
    action (CollectedAllData ud ld cbd)           (ProvideLoanDetails ld')       = (_, _)
    action (CollectedAllData ud ld cbd)           (ProvideCreditBureauData cbd') = (_, _)

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
    action :: RiskEvent -> IO [RiskCommand]
    action (UserDataRegistered ud)        = _
    action (LoanDetailsProvided ld)       = _
    action (CreditBureauDataReceived cbd) = _

riskManagerApplication :: Application IO RiskCommand RiskEvent ReceivedData
riskManagerApplication = Application riskAggregate (Just riskPolicy) riskProjection
