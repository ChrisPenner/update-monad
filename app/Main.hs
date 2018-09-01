{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Function ((&))
import Data.Monoid
import Text.Read
import Update

main :: IO ()
main = auditUpdateT loop (BankBalance 0) >>= print

testGame :: BankBalance
testGame =
  applyAction [Deposit 30, Deposit 30, Withdraw 50, Deposit 10] (BankBalance 0)

loop :: UpdateT BankBalance [Event] IO ()
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

instance ApplyAction [Event] BankBalance where
  applyAction events balance =
    balance & (appEndo . foldMap (Endo . processTransaction) $ events)

processTransaction :: Event -> BankBalance -> BankBalance
processTransaction (Deposit n) (BankBalance i) = BankBalance (i + n)
processTransaction (Withdraw n) (BankBalance i) = BankBalance (i - n)

newtype BankBalance =
  BankBalance Int
  deriving (Eq, Ord, Show)

data Event
  = Deposit Int
  | Withdraw Int
  deriving (Eq, Ord, Show)
