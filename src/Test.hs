module Main where

import Network.Simple.TCP

import RLNetwork

main = do
  -- Init
  glueConnect rlinit

rlinit :: (Socket, SockAddr) -> IO ()
rlinit (sock, addr) = do
  doCallWithNoParams sock (fromIntegral kRLInit)
