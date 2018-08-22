module Main where

import Control.Monad
import Control.Monad.IO.Class
import FreeUpdate

main :: IO ()
main = evalUpdateT loop stepGame (GameState 0)

stepGame :: GameState -> [Event] -> GameState
stepGame (GameState i) (Inc:_) = GameState (i + 1)
stepGame (GameState i) (Dec:_) = GameState (i - 1)
stepGame g [] = g

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
  action . pure $
    case c of
      'k' -> Inc
      _ -> Dec
  when (c /= 'q') loop
