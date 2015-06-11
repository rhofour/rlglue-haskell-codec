module Main where

import qualified Data.ByteString as BS
import Network.Simple.TCP

import RLExperiment

main = do
  runExperiment doExperiments

doExperiments :: (Socket, SockAddr) -> BS.ByteString -> IO ()
doExperiments (sock, addr) taskSpec =
  do
    putStrLn ("Sent task spec: " ++ (show taskSpec))
