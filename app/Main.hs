{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Function ((&))
import Data.Monoid
import Text.Read
import UpdateT

main :: IO ()
main = auditUpdateT loop (BankBalance 0) >>= print

testGame :: BankBalance
testGame =
  applyAction [Deposit 30, Deposit 30, Withdraw 50, Deposit 10] (BankBalance 0)

loop :: UpdateT [AccountAction] BankBalance IO ()
loop = do
  ln <- liftIO getLine
  unless (filter (not . isSpace) ln == "q") $ do
    let mAmt = readMaybe ln
    case mAmt of
      Nothing -> liftIO (print "please enter a number")
      Just amt -> do
        let evt =
              if amt > 0
                then Deposit amt
                else Withdraw (abs amt)
        liftIO (print evt)
        putAction [evt]
        getState >>= liftIO . print
    loop

instance ApplyAction [AccountAction] BankBalance where
  applyAction actions balance =
    let allTransactions :: BankBalance -> BankBalance
        allTransactions =
          appEndo $ foldMap (Endo . processTransaction) (reverse actions)
     in allTransactions balance

processTransaction :: AccountAction -> BankBalance -> BankBalance
processTransaction (Deposit n) (BankBalance b) = BankBalance (b + n)
processTransaction (Withdraw n) (BankBalance b) = BankBalance (b - n)
processTransaction ApplyInterest (BankBalance b) = BankBalance (applyInterest b)

-- This is a gross oversimplification...
-- I really hope my bank does something smarter
-- We (kinda sorta) add 10% interest, truncating any cents.
-- Who likes keeping track of change anyways ¯\_(ツ)_/¯
applyInterest :: Int -> Int
applyInterest balance = truncate (fromIntegral balance * 1.1)

newtype BankBalance =
  BankBalance Int
  deriving (Eq, Ord, Show)

data AccountAction
  = Deposit Int
  | Withdraw Int
  | ApplyInterest
  deriving (Eq, Ord, Show)

useATM :: Update [AccountAction] BankBalance BankBalance
useATM = do
  putAction [Deposit 20]
  putAction [Deposit 30]
  putAction [ApplyInterest]
  putAction [Withdraw 10]
  getState

testBankSystem :: Bool
testBankSystem =
  applyAction
    [Deposit 20, Deposit 30, ApplyInterest, Withdraw 10]
    (BankBalance 0) ==
  BankBalance 45
