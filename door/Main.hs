module Main where

import DDD (runApplication)
import Door (DoorCommand(..), doorApplication)

-- base
import Data.Functor.Identity (runIdentity)

main :: IO ()
main =
  print . runIdentity $ runApplication doorApplication [Open, Open, Open, Close, Close, Knock] -- 2
