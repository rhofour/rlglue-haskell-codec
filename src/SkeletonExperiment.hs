module Main where

import qualified Data.ByteString as BS
import Data.Binary.Put
import Network.Simple.TCP

import RLExperiment
import RLNetwork

main = do
  runExperiment doExperiments

doExperiments :: (Socket, SockAddr) -> IO ()
doExperiments (sock, addr) =
  do
    taskSpec <- initExperiment sock
    putStrLn ("Sent task spec: " ++ (show taskSpec))

    putStrLn "\n----------Sending some sample messages----------"
    resp <- sendAgentMessageStr sock "what is your name?"
    putStrLn ("Agent responded to \"what is your name?\" with: " ++ (show resp))

    resp <- sendAgentMessageStr sock "If at first you don't succeed; call it version 1.0"
    putStrLn ("Agent responded to \"If at first you don't succeed; call it version 1.0?\" with: " ++ (show resp))

    resp <- sendEnvMessageStr sock "what is your name?"
    putStrLn ("Environment responded to \"what is your name?\" with: " ++ (show resp))

    resp <- sendEnvMessageStr sock "If at first you don't succeed; call it version 1.0"
    putStrLn ("Environment responded to \"If at first you don't succeed; call it version 1.0?\" with: " ++ (show resp))

    putStrLn "\n----------Running a few episodes----------"

    runEpisode sock 100
    runEpisode sock 100
    runEpisode sock 100
    runEpisode sock 100
    runEpisode sock 100
    runEpisode sock 1
    -- Run one without a limit
    runEpisode sock 0

    cleanupExperiment sock

