module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import RLEnvironment
import RLNetwork

main = do
  loadEnvironment (Environment onInit onStart onStep onCleanup onMsg) ()
  return ()

onInit :: StateT () IO BS.ByteString
onInit = do
  lift $ putStrLn "Initialized."
  lift $ return (BSC.pack "VERSION RL-Glue-3.0 PROBLEMTYPE episodic DISCOUNTFACTOR 1.0 OBSERVATIONS INTS (0 20)  ACTIONS INTS (0 1)  REWARDS (-1.0 1.0)  EXTRA skeleton_environment(Python) by Brian Tanner.")

onStart :: StateT () IO ()
onStart = lift $ putStrLn "Started."

onStep :: StateT () IO ()
onStep = lift $ putStrLn "Stepped."

onCleanup :: StateT () IO ()
onCleanup = lift $ putStrLn "Cleaned up."

onMsg :: StateT () IO ()
onMsg = lift $ putStrLn "Message received."
