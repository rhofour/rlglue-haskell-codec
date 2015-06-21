module RL_Glue.Agent (
  Agent(Agent), loadAgent
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Version (showVersion)
import Data.Word
import Network.Simple.TCP
import System.Exit

import Paths_rlglue (version)
import RL_Glue.Network

data Agent a = Agent
  { onAgentInit :: BS.ByteString -> StateT a IO ()
  , onAgentStart :: Observation -> StateT a IO Action
  , onAgentStep :: (Reward, Observation) -> StateT a IO Action
  , onAgentEnd :: Reward -> StateT a IO ()
  , onAgentCleanup :: (StateT a IO ())
  , onAgentMessage :: (BS.ByteString -> StateT a IO BS.ByteString)
  }

loadAgent ::  Agent a -> a -> IO ()
loadAgent agent initState =
  let 
    func (sock, addr) =
      do
        -- Initial setup
        putStrLn ("RL-Glue Haskell Agent Codec (Version " ++ (showVersion version) ++ ")")
        let bs = runPut (putWord32be kAgentConnection >> putWord32be (0 :: Word32))
        sendLazy sock bs

        -- Run event loop
        evalStateT (eventLoop agent sock) initState
  in
    glueConnect func

eventLoop :: Agent a -> Socket -> StateT a IO ()
eventLoop agent sock = do
  x <- lift $ runMaybeT (getAgentState sock)
  case x of
    Nothing -> do
      lift $ putStrLn "Error: Failed to receive state."
      lift $ exitWith (ExitFailure 1)
    Just (state, size) -> do
      if state == kRLTerm
        then return ()
        else do
          handleState sock agent state
          eventLoop agent sock

handleState :: Socket -> Agent a -> Word32 -> StateT a IO ()
handleState sock agent state
  | state == kAgentInit = do
    taskSpec <- lift $ getStringOrDie "Error: Could not get task spec" sock
    onAgentInit agent taskSpec
    let packedMsg = runPut (
          putWord32be kAgentInit >>
          putWord32be 0)
    sendLazy sock packedMsg
  | state == kAgentStart = do
    obs <- lift $ getObservationOrDie sock
    action <- onAgentStart agent obs
    let size = sizeOfAction action
    let packedMsg = runPut (
          putWord32be kAgentStart >>
          putWord32be (fromIntegral size) >>
          putAction action)
    sendLazy sock packedMsg
  | state == kAgentStep = do
    rewardObs <- lift $ getRewardObservationOrDie sock
    action <- onAgentStep agent rewardObs
    let size = sizeOfAction action
    let packedMsg = runPut (
          putWord32be kAgentStep >>
          putWord32be (fromIntegral size) >>
          putAction action)
    sendLazy sock packedMsg
  | state == kAgentEnd = do
    reward <- lift $ getRewardOrDie sock
    onAgentEnd agent reward
    let packedMsg = runPut (
          putWord32be kAgentEnd >>
          putWord32be 0)
    sendLazy sock packedMsg
  | state == kAgentCleanup = do
    onAgentCleanup agent
    let packedMsg = runPut (
          putWord32be kAgentCleanup >>
          putWord32be 0)
    sendLazy sock packedMsg
  | state == kAgentMessage = do
    msg <- lift $ getStringOrDie "Error: Could not read message" sock
    resp <- onAgentMessage agent msg
    let packedMsg = runPut (
          putWord32be kAgentMessage >>
          if BS.null resp
            then putWord32be 4 >> putWord32be 0
            else 
              putWord32be (fromIntegral $ 4 + (BS.length resp)) >>
              putString resp)
    sendLazy sock packedMsg
  | state == kRLTerm = lift $ return ()
  | otherwise  = do
    lift $ putStrLn $ "Error: Unknown state: " ++ (show state)
    lift $ exitWith (ExitFailure 1)

getAgentState :: Socket -> MaybeT IO (Word32, Word32)
getAgentState sock = do
  bs <- MaybeT $ recv sock (4*2)
  return $ runGet (parseBytes) (LBS.fromStrict bs)
  where
    parseBytes = do
      envState <- getWord32be
      dataSize <- getWord32be
      return (envState, dataSize)
