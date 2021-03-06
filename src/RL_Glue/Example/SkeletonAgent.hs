module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import System.Exit
import System.Random

import RL_Glue.Agent
import RL_Glue.Network

main = loadAgent (Agent onInit onStart onStep onEnd onCleanup onMessage) ()

onInit :: BS.ByteString -> StateT () IO ()
onInit taskSpec = return ()

onStart :: Observation -> StateT () IO Action
onStart obs = do
  dir <- lift $ getStdRandom (randomR (0,1))
  return (Action $ RLAbstractType [dir] [] BS.empty)

onStep :: (Reward, Observation) -> StateT () IO Action
onStep (reward, obs) = do
  dir <- lift $ getStdRandom (randomR (0,1))
  return (Action $ RLAbstractType [dir] [] BS.empty)

onEnd :: Reward -> StateT () IO ()
onEnd reward = return ()

onCleanup :: (StateT () IO ())
onCleanup = return ()

onMessage :: BS.ByteString -> StateT () IO BS.ByteString
onMessage msg =
  return $ BSC.pack $ if msg == BSC.pack "what is your name?"
    then "my name is skeleton_agent, Haskell edition!"
    else "I don't know how to respond to your message"
