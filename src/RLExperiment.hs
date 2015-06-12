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

runExperiment :: ((Socket, SockAddr) -> BS.ByteString -> IO ()) -> IO ()
runExperiment func =
  let 
    func' (sock, addr) =
      do
        putStrLn ("RL-Glue Haskell Experiment Codec (Version " ++ (showVersion version) ++ ")")
        doCallWithNoParams sock kExperimentConnection

        -- Initialization
        doCallWithNoParams sock kRLInit
        confirmState sock kRLInit
        taskSpec <- runMaybeT (getString sock)

        -- Do stuff
        case taskSpec of
          Nothing -> putStrLn "Error: Could not read task spec"
          Just x -> func (sock, addr) x

        -- Cleanup
        doCallWithNoParams sock kRLCleanup
        confirmState sock kRLCleanup
  in
    glueConnect func'

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
