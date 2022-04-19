{-# LANGUAGE DuplicateRecordFields #-}

module RiskManager where

import DDD
import Machines

-- text
import Data.Text (Text)

newtype Name = Name Text

newtype Surname = Surname Text

newtype TaxCode = TaxCode Text

data UserData = UserData
  { name :: Name
  , surname :: Surname
  , taxCode :: TaxCode
  }

newtype Amount = EuroCents Int

newtype InstalmentsNumber = InstalmentsNumber Int

data LoanDetails = LoanDetails
  { amount      :: Amount
  , instalments :: InstalmentsNumber
  }

newtype MissedPaymentDeadlines = MissedPaymentDeadlines Int

data CreditBureauData = CreditBureauData
  { missedPaymentDeadlines :: Int
  , arrears                :: Amount
  }

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
interactWithCreditBureau = undefined -- some IO operation to retrieve the credi bureau data

riskPolicy :: Policy IO RiskEvent RiskCommand
riskPolicy = Policy $ statelessT action
  where
    action :: RiskEvent -> IO [RiskCommand]
    action (UserDataRegistered ud)        = pure . ProvideCreditBureauData <$> interactWithCreditBureau ud
    action (LoanDetailsProvided ld)       = pure []
    action (CreditBureauDataReceived cbd) = pure []
