module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
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

    evalStateT
      (do
        prettyRunEpisode sock 100
        prettyRunEpisode sock 100
        prettyRunEpisode sock 100
        prettyRunEpisode sock 100
        prettyRunEpisode sock 100
        prettyRunEpisode sock 1
        -- Run one without a limit
        prettyRunEpisode sock 0) 1
    cleanupExperiment sock

    putStrLn "\n----------Stepping through an episode----------"
    taskSpec <- initExperiment sock

    -- Start an episode
    (Observation (RLAbstractType o _ _), Action (RLAbstractType a _ _)) <- startEpisode sock
    putStrLn $ "First observation and action were: " ++ (show (head o)) ++ " and: " ++ (show (head a))

    -- Step until episode ends
    loopUntil $ do
      (_, _, _, terminal) <- stepEpisode sock
      return (terminal > 0)

    putStrLn "\n----------Summary----------"
    totalSteps <- getNumSteps sock
    totalReward <- getReturn sock
    putStrLn $ "It ran for " ++ (show totalSteps) ++ " steps, total reward was: " ++ (show totalReward)

    cleanupExperiment sock

prettyRunEpisode :: Socket -> Int -> StateT Int IO ()
prettyRunEpisode sock steps =
  do
    terminal <- lift $ runEpisode sock steps
    totalSteps <- lift $ getNumSteps sock
    totalReward <- lift $ getReturn sock

    episodeNum <- get
    lift $ putStrLn $ "Episode " ++ (show episodeNum) ++ "\t " ++ (show totalSteps) ++ " steps \t" ++
      (show totalReward) ++ " total reward\t" ++ (show terminal) ++ " natural end"
    modify (+1)

loopUntil :: IO Bool -> IO ()
loopUntil f = do
  x <- f
  if x then return () else loopUntil f
