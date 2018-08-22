module Main where

import Control.Monad
import Control.Monad.IO.Class
import FreeUpdate

main :: IO ()
main = execUpdateT loop stepGame (GameState 0) >>= print

stepGame :: GameState -> Event -> GameState
stepGame (GameState i) Inc = GameState (i + 1)
stepGame (GameState i) Dec = GameState (i - 1)

newtype GameState =
  GameState Int
  deriving (Eq, Ord, Show)

data Event
  = Inc
  | Dec

loop :: FreeUpdateT GameState Event IO ()
loop = do
  GameState n <- currentState
  liftIO $ print n
  c <- liftIO $ getChar <* putStrLn ""
  case c of
    'q' -> pure ()
    'k' -> action Inc >> loop
    _ -> action Dec >> loop

addLength :: FreeUpdateT s String IO ()
addLength = liftIO getLine >>= action

prog :: FreeUpdateT s String IO ()
prog = do
  addLength
  addLength
  addLength

runProgFake :: FreeUpdateT [String] String IO () -> IO ()
runProgFake u = do
  collection <- collectUpdateT u (flip (:)) []
  print collection

runProgReal :: FreeUpdateT Int String IO () -> IO ()
runProgReal u = do
  cnt <- execUpdateT u (\i s -> i + (length $ s)) 0
  print cnt
