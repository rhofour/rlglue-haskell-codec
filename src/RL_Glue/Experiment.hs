module RL_Glue.Experiment (
  runExperiment, initExperiment, cleanupExperiment, runEpisode, startEpisode,
  stepEpisode, getNumSteps, getNumEpisodes, getReturn
  ) where

import Control.Monad.Trans.Maybe
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Version (showVersion)
import Data.Word
import Network.Simple.TCP
import System.Exit

import Paths_rlglue (version)
import RL_Glue.Network

runExperiment :: ((Socket, SockAddr) -> IO ()) -> IO ()
runExperiment func =
  let 
    func' (sock, addr) =
      do
        putStrLn ("RL-Glue Haskell Experiment Codec (Version " ++ showVersion version ++ ")")
        let bs = runPut (putWord32be kExperimentConnection >> putWord32be (0 :: Word32))
        sendLazy sock bs

        -- Actually do things
        func (sock, addr)
  in
    glueConnect func'

initExperiment :: Socket -> IO BS.ByteString
initExperiment sock =
  do
    doCallWithNoParams sock kRLInit
    taskSpec <- runMaybeT (getString sock)
    case taskSpec of
      Nothing -> do
        putStrLn "Error: Could not read task spec"
        exitWith (ExitFailure 1)
      Just x -> return x

cleanupExperiment :: Socket -> IO ()
cleanupExperiment sock = doCallWithNoParams sock kRLCleanup

runEpisode :: Socket -> Int -> IO Int
runEpisode sock stepLimit =
  do
    let 
      packedMsg = 
        runPut (
          putWord32be kRLEpisode >>
          putWord32be (fromIntegral kIntSize) >>
          putWord32be (fromIntegral stepLimit))
    sendLazy sock packedMsg
    confirmState sock kRLEpisode
    respBs <- recv sock 4
    case respBs of
      Nothing -> do
        putStrLn "Error: Could not read episode status from network"
        exitWith (ExitFailure 1)
      Just x -> return $ fromIntegral $ runGet getWord32be (LBS.fromStrict x)

startEpisode :: Socket -> IO (Observation, Action)
startEpisode sock =
  do
    doCallWithNoParams sock kRLStart
    x <- runMaybeT (do
      obs <- getObservation sock
      act <- getAction sock
      return (obs, act))
    case x of
      Nothing -> do
        putStrLn "Error: Could not start episode over network"
        exitWith (ExitFailure 1)
      Just x' -> return x'

stepEpisode :: Socket -> IO (Reward, Observation, Action, Terminal)
stepEpisode sock =
  do
    doCallWithNoParams sock kRLStep
    let parseBytes = do
          terminal <- getWord32be
          reward <- getFloat64be
          return (fromIntegral terminal, reward)
    x <- runMaybeT (do
      bs <- MaybeT $ recv sock (4+8)
      let (terminal, reward) = runGet parseBytes (LBS.fromStrict bs)
      obs <- getObservation sock
      act <- getAction sock
      return (reward, obs, act, terminal))
    case x of
      Nothing -> do
        putStrLn "Error: Could not step episode over network"
        exitWith (ExitFailure 1)
      Just x' -> return x'
    where

getNetworkValue :: Word32 -> (Socket -> MaybeT IO a) -> String -> Socket -> IO a
getNetworkValue byte  f errMsg sock =
  do
    doCallWithNoParams sock byte
    x <- runMaybeT (f sock)
    case x of
      Nothing -> do
        putStrLn errMsg
        exitWith (ExitFailure 1)
      Just x' -> return x'

getNumSteps = getNetworkValue kRLNumSteps getInt "Error: Could not read number of steps from network."

getNumEpisodes = getNetworkValue kRLNumEpisodes getInt "Error: Could not read number of episodes from network."

getReturn = getNetworkValue kRLReturn getDouble "Error: Could not read return from network."
