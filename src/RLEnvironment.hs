module RLEnvironment where

import Data.Binary.Put
import Data.Version (showVersion)
import Data.Word
import Network.Simple.TCP

import Paths_rlglue_haskell_codec (version)
import RLNetwork

data Environment = Environment

loadEnvironment ::  Environment -> IO ()
loadEnvironment env =
  let 
    func (sock, addr) =
      do
        putStrLn ("RL-Glue Haskell Environment Codec (Version " ++ (showVersion version) ++ ")")
        let bs = runPut (putWord32be kEnvironmentConnection >> putWord32be (0 :: Word32))
        sendLazy sock bs
  in
    glueConnect func

loopUntil :: IO Bool -> IO ()
loopUntil f = do
  x <- f
  if x then return () else loopUntil f
