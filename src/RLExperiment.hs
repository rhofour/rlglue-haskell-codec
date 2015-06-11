module RLExperiment where

import Data.Version (showVersion)
import Network.Simple.TCP

import Paths_rlglue_haskell_codec (version)
import RLNetwork

runExperiment :: ((Socket, SockAddr) -> IO r) -> IO r
runExperiment func =
  let
    func' (sock, addr) = do
      putStrLn ("RL-Glue Haskell Experiment Codec (Version " ++ (showVersion version) ++ ")")
      doCallWithNoParams sock (fromIntegral kExperimentConnection)
      func (sock, addr)
  in
    glueConnect func'
