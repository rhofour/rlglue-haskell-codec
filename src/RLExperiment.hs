module RLExperiment where

import Control.Monad.Trans.Maybe
import Data.Version (showVersion)
import Network.Simple.TCP

import Paths_rlglue_haskell_codec (version)
import RLNetwork

runExperiment :: ((Socket, SockAddr) -> IO r) -> IO ()
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
        putStrLn ("Task Spec: " ++ (show taskSpec))

        -- Do stuff
        func (sock, addr)

        -- Cleanup
        doCallWithNoParams sock (fromIntegral kRLCleanup)
        confirmState sock kRLCleanup
  in
    glueConnect func'
