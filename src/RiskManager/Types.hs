{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RiskManager.Types where

-- base
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
