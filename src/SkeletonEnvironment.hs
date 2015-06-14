module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import RLEnvironment
import RLNetwork

main = do
  loadEnvironment (Environment onInit onStart onStep onCleanup onMsg) ()
  return ()

onInit :: StateT () IO ()
onInit = lift $ putStrLn "Initialized."

onStart :: StateT () IO ()
onStart = lift $ putStrLn "Started."

onStep :: StateT () IO ()
onStep = lift $ putStrLn "Stepped."

onCleanup :: StateT () IO ()
onCleanup = lift $ putStrLn "Cleaned up."

onMsg :: StateT () IO ()
onMsg = lift $ putStrLn "Message received."
