module RL_Glue.Environment (
  Environment(Environment), loadEnvironment
  ) where

import Control.Monad (unless)
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

data Environment a = Environment
  { onEnvInit :: StateT a IO BS.ByteString
  , onEnvStart :: StateT a IO Observation
  , onEnvStep :: Action -> StateT a IO (Terminal, Reward, Observation)
  , onEnvCleanup :: StateT a IO ()
  , onEnvMessage :: BS.ByteString -> StateT a IO BS.ByteString
  }

loadEnvironment ::  Environment a -> a -> IO ()
loadEnvironment env initState =
  let 
    func (sock, addr) =
      do
        -- Initial setup
        putStrLn ("RL-Glue Haskell Environment Codec (Version " ++ showVersion version ++ ")")
        let bs = runPut (putWord32be kEnvironmentConnection >> putWord32be (0 :: Word32))
        sendLazy sock bs

        -- Run event loop
        evalStateT (eventLoop env sock) initState
  in
    glueConnect func

eventLoop :: Environment a -> Socket -> StateT a IO ()
eventLoop env sock = do
  x <- lift $ runMaybeT (getEnvState sock)
  case x of
    Nothing -> do
      lift $ putStrLn "Error: Failed to receive state."
      lift $ exitWith (ExitFailure 1)
    Just (state, size) ->
      unless (state == kRLTerm) $ do
        handleState sock env state
        eventLoop env sock

handleState :: Socket -> Environment a -> Word32 -> StateT a IO ()
handleState sock env state
  | state == kEnvInit = do
    taskSpec <- onEnvInit env
    let packedMsg = runPut (
          putWord32be kEnvInit >>
          putWord32be (fromIntegral (4 + BS.length taskSpec)) >>
          putString taskSpec)
    sendLazy sock packedMsg
  | state == kEnvStart = do
    obs <- onEnvStart env
    let size = sizeOfObs obs
    let packedMsg = runPut (
          putWord32be kEnvStart >>
          putWord32be (fromIntegral size) >>
          putObservation obs)
    sendLazy sock packedMsg
  | state == kEnvStep = do
    x <- lift $ runMaybeT (getAction sock)
    case x of
      Nothing -> lift $ do
        putStrLn "Error: Could not read action over network"
        exitWith (ExitFailure 1)
      Just action -> do
        terminalRewardObs <- onEnvStep env action
        let size = sizeOfRewardObs terminalRewardObs
        let packedMsg = runPut (
              putWord32be kEnvStep >>
              putWord32be (fromIntegral size) >>
              putTerminalRewardObs terminalRewardObs)
        sendLazy sock packedMsg
  | state == kEnvCleanup = do
    onEnvCleanup env
    let packedMsg = runPut (
          putWord32be kEnvCleanup >>
          putWord32be 0)
    sendLazy sock packedMsg
  | state == kEnvMessage = do
    x <- lift $ runMaybeT (getString sock)
    case x of
      Nothing -> lift $ do
        putStrLn "Error: Could not read message"
        exitWith (ExitFailure 1)
      Just msg' -> do
        resp <- onEnvMessage env msg'
        let packedMsg = runPut (
              putWord32be kEnvMessage >>
              if BS.null resp
                then putWord32be 4 >> putWord32be 0
                else 
                  putWord32be (fromIntegral $ 4 + BS.length resp) >>
                  putString resp)
        sendLazy sock packedMsg
  | state == kRLTerm = lift $ return ()
  | otherwise  = do
    lift $ putStrLn $ "Error: Unknown state: " ++ show state
    lift $ exitWith (ExitFailure 1)

getEnvState :: Socket -> MaybeT IO (Word32, Word32)
getEnvState sock = do
  bs <- MaybeT $ recv sock (4*2)
  return $ runGet parseBytes (LBS.fromStrict bs)
  where
    parseBytes = do
      envState <- getWord32be
      dataSize <- getWord32be
      return (envState, dataSize)
