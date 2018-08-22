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
    'k' -> (action . pure $ Inc) >> loop
    _ -> (action . pure $ Dec) >> loop
