module RLExperiment where

import Control.Monad.Trans.Maybe
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Version (showVersion)
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
