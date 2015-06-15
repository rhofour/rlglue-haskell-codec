module RLAgent where

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

import Paths_rlglue_haskell_codec (version)
import RLNetwork

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
    taskSpec <- lift $ getStringOrDie sock "Error: Could not get task spec"
    onAgentInit agent taskSpec
    let packedMsg = runPut (
          putWord32be kAgentInit >>
          putWord32be 0)
    sendLazy sock packedMsg
  -- | state == kAgentStart = do
  --   obs <- onAgentStart env
  --   let size = sizeOfObs obs
  --   let packedMsg = runPut (
  --         putWord32be kAgentStart >>
  --         putWord32be (fromIntegral size) >>
  --         putObservation obs)
  --   sendLazy sock packedMsg
  -- | state == kAgentStep = do
  --   x <- lift $ runMaybeT (fmap Action (getAbstractType sock))
  --   case x of
  --     Nothing -> lift $ do
  --       putStrLn "Error: Could not read action over network"
  --       exitWith (ExitFailure 1)
  --     Just action -> do
  --       terminalRewardObs <- onAgentStep env action
  --       let size = sizeOfRewardObs terminalRewardObs
  --       let packedMsg = runPut (
  --             putWord32be kAgentStep >>
  --             putWord32be (fromIntegral size) >>
  --             putTerminalRewardObs terminalRewardObs)
  --       sendLazy sock packedMsg
  -- | state == kAgentCleanup = do
  --   onAgentCleanup env
  --   let packedMsg = runPut (
  --         putWord32be kAgentCleanup >>
  --         putWord32be 0)
  --   sendLazy sock packedMsg
  -- | state == kAgentMessage = do
  --   x <- lift $ runMaybeT (getString sock)
  --   case x of
  --     Nothing -> lift $ do
  --       putStrLn "Error: Could not read message"
  --       exitWith (ExitFailure 1)
  --     Just msg' -> do
  --       resp <- onAgentMessage env msg'
  --       let packedMsg = runPut (
  --             putWord32be kAgentMessage >>
  --             if BS.null resp
  --               then putWord32be 4 >> putWord32be 0
  --               else 
  --                 putWord32be (fromIntegral $ 4 + (BS.length resp)) >>
  --                 putString resp)
  --       sendLazy sock packedMsg
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
