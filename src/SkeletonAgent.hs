module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import System.Exit

import RLAgent
import RLNetwork

main = do
  loadAgent (Agent onInit onStart onStep onEnd onCleanup onMessage) ()
  return ()

onInit :: BS.ByteString -> StateT () IO ()
onInit taskSpec = return ()

onStart :: Observation -> StateT () IO Action
onStart obs = return (Action $ RLAbstractType [] [] BS.empty)

onStep :: (Reward, Observation) -> StateT () IO Action
onStep (reward, obs) = return (Action $ RLAbstractType [] [] BS.empty)

onEnd :: Reward -> StateT () IO ()
onEnd reward = return ()

onCleanup :: (StateT () IO ())
onCleanup = return ()

onMessage :: (BS.ByteString -> StateT () IO BS.ByteString)
onMessage msg =
  return $ if msg == (BSC.pack "what is your name?")
    then (BSC.pack "my name is skeleton_agent, Haskell edition!")
    else (BSC.pack "I don't know how to respond to your message")
