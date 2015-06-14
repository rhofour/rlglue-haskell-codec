module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import System.Exit

import RLEnvironment
import RLNetwork

main = do
  loadEnvironment (Environment onInit onStart onStep onCleanup onMsg) 10
  return ()

onInit :: StateT Int IO BS.ByteString
onInit = do
  lift $ putStrLn "Initialized."
  lift $ return (BSC.pack "VERSION RL-Glue-3.0 PROBLEMTYPE episodic DISCOUNTFACTOR 1.0 OBSERVATIONS INTS (0 20)  ACTIONS INTS (0 1)  REWARDS (-1.0 1.0)  EXTRA skeleton_environment(Python) by Brian Tanner.")

onStart :: StateT Int IO Observation
onStart = do
  lift $ putStrLn "Started."
  return (Observation (RLAbstractType [10] [] BS.empty))

onStep :: Action -> StateT Int IO (Terminal, Reward, Observation)
onStep (Action (RLAbstractType (dir:_) _ _)) = do
  lift $ putStrLn "Steping."
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
        return (1, 1, Observation (RLAbstractType [0] [] BS.empty))
      | otherwise = return (0, 0, Observation (RLAbstractType [x] [] BS.empty))

onCleanup :: StateT Int IO ()
onCleanup = lift $ putStrLn "Cleaned up."

onMsg :: StateT Int IO ()
onMsg = lift $ putStrLn "Message received."
