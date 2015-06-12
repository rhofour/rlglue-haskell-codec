module RLExperiment where

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

import Paths_rlglue_haskell_codec (version)
import RLNetwork

runExperiment :: ((Socket, SockAddr) -> IO ()) -> IO ()
runExperiment func =
  let 
    func' (sock, addr) =
      do
        putStrLn ("RL-Glue Haskell Experiment Codec (Version " ++ (showVersion version) ++ ")")
        doCallWithNoParams sock kExperimentConnection

        -- Actually do things
        func (sock, addr)
  in
    glueConnect func'

initExperiment :: Socket -> IO BS.ByteString
initExperiment sock =
  do
    doCallWithNoParams sock kRLInit
    confirmState sock kRLInit
    taskSpec <- runMaybeT (getString sock)
    case taskSpec of
      Nothing -> do
        putStrLn "Error: Could not read task spec"
        exitWith (ExitFailure 1)
      Just x -> return x

cleanupExperiment :: Socket -> IO ()
cleanupExperiment sock =
  do
    doCallWithNoParams sock kRLCleanup
    confirmState sock kRLCleanup

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
    respBs <- recv sock (4)
    case respBs of
      Nothing -> do
        putStrLn "Error: Could not read episode status from network"
        exitWith (ExitFailure 1)
      Just x -> return $ fromIntegral $ runGet getWord32be (LBS.fromStrict x)

startEpisode :: Socket -> IO (Observation, Action)
startEpisode sock =
  do
    doCallWithNoParams sock kRLStart
    confirmState sock kRLStart
    x <- runMaybeT (do
      abs1 <- getAbstractType sock
      let obs = Observation abs1 
      abs2 <- getAbstractType sock
      let act = Action $ abs2
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
    confirmState sock kRLStep
    x <- runMaybeT (do
      bs <- MaybeT $ recv sock (4+8)
      let (terminal, reward) = runGet parseBytes (LBS.fromStrict bs)
      abs1 <- getAbstractType sock
      let obs = Observation abs1 
      abs2 <- getAbstractType sock
      let act = Action $ abs2
      return (reward, obs, act, terminal))
    case x of
      Nothing -> do
        putStrLn "Error: Could not step episode over network"
        exitWith (ExitFailure 1)
      Just x' -> return x'
    where
      parseBytes = do
        terminal <- getWord32be
        reward <- getFloat64be
        return (fromIntegral terminal, reward)

getNetworkValue :: Word32 -> (Socket -> MaybeT IO a) -> String -> Socket -> IO a
getNetworkValue byte  f errMsg sock =
  do
    doCallWithNoParams sock byte
    confirmState sock byte
    x <- runMaybeT (f sock)
    case x of
      Nothing -> do
        putStrLn errMsg
        exitWith (ExitFailure 1)
      Just x' -> return x'

numSteps :: Socket -> IO Int
numSteps = getNetworkValue kRLNumSteps getInt "Error: Could not read number of steps from network."
