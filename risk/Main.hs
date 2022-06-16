{-# LANGUAGE OverloadedStrings #-}

module Main where

import DDD
import RiskManager
import RiskManager.Types

myUserData :: UserData
myUserData = UserData
  { name = "Marco"
  , surname = "Perone"
  , taxCode = "PRNMRC83S14C957V"
  }

myLoanDetails :: LoanDetails
myLoanDetails = LoanDetails
  { amount      = EuroCents 10000
  , instalments = InstalmentsNumber 12
  }

main :: IO ()
main = print =<< runApplication riskManagerApplication
  [ RegisterUserData myUserData
  , ProvideLoanDetails myLoanDetails
  ]