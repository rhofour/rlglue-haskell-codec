module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import RLEnvironment
import RLNetwork

main = do
  loadEnvironment (Environment onInit) ()
  return ()

onInit :: StateT () IO ()
onInit = lift $ putStrLn "Initialized"
