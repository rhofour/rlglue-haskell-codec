module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import System.Exit

import RL_Glue.Environment
import RL_Glue.Network

main = do
  loadEnvironment (Environment onInit onStart onStep onCleanup onMsg) 10
  return ()

onInit :: StateT Int IO BS.ByteString
onInit = do
  lift $ return (BSC.pack "VERSION RL-Glue-3.0 PROBLEMTYPE episodic DISCOUNTFACTOR 1.0 OBSERVATIONS INTS (0 20)  ACTIONS INTS (0 1)  REWARDS (-1.0 1.0)  EXTRA skeleton_environment(Haskell) by Richard Hofer.")

onStart :: StateT Int IO Observation
onStart = do
  put 10
  return (Observation (RLAbstractType [10] [] BS.empty))

onStep :: Action -> StateT Int IO (Terminal, Reward, Observation)
onStep (Action (RLAbstractType (dir:_) _ _)) = do
  case dir of
    0 -> modify (subtract 1)
    1 -> modify (+1)
    _ -> lift $ do
      putStrLn "Error: non-binary direction received"
      exitWith (ExitFailure 1)
  state <- get
  handleState state
  where
    handleState x
      | x <= 0 = do
        put 0
        return (1, -1, Observation (RLAbstractType [0] [] BS.empty))
      | x >= 20 = do
        put 20
        return (1, 1, Observation (RLAbstractType [20] [] BS.empty))
      | otherwise = return (0, 0, Observation (RLAbstractType [x] [] BS.empty))

onCleanup :: StateT Int IO ()
onCleanup = return ()

onMsg :: BS.ByteString -> StateT Int IO BS.ByteString
onMsg msg = do
  return $ if msg == (BSC.pack "what is your name?")
    then (BSC.pack "my name is skeleton_environment, Haskell edition!")
    else (BSC.pack "I don't know how to respond to your message")
