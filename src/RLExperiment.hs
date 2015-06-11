module RLExperiment where

import Control.Monad.Trans.Maybe
import qualified Data.ByteString as BS
import Data.Version (showVersion)
import Network.Simple.TCP

import Paths_rlglue_haskell_codec (version)
import RLNetwork

runExperiment :: ((Socket, SockAddr) -> BS.ByteString -> IO ()) -> IO ()
runExperiment func =
  let 
    func' (sock, addr) =
      do
        putStrLn ("RL-Glue Haskell Experiment Codec (Version " ++ (showVersion version) ++ ")")
        doCallWithNoParams sock (fromIntegral kExperimentConnection)

        -- Initialization
        doCallWithNoParams sock (fromIntegral kRLInit)
        confirmState sock kRLInit
        taskSpec <- runMaybeT (getString sock)

        -- Do stuff
        case taskSpec of
          Nothing -> putStrLn "Error: Could not read task spec"
          Just x -> func (sock, addr) x

        -- Cleanup
        doCallWithNoParams sock (fromIntegral kRLCleanup)
        confirmState sock kRLCleanup
  in
    glueConnect func'
