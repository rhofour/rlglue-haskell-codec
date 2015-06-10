module RLNetwork where

import Control.Exception
import System.Environment
import System.IO.Error

import Network.Simple.TCP

-- Connection types
kExperimentConnection  = 1
kAgentConnection       = 2
kEnvironmentConnection = 3

kAgentInit      = 4
kAgentStart     = 5
kAgentStep      = 6
kAgentEnd       = 7
kAgentCleanup   = 8
kAgentMessage   = 10

kEnvInit        = 11
kEnvStart       = 12
kEnvStep        = 13
kEnvCleanup     = 14
kEnvMessage     = 19

kRLInit         = 20
kRLStart        = 21
kRLStep         = 22
kRLCleanup      = 23
kRLReturn       = 24
kRLNumSteps     = 25
kRLNumEpisodes  = 26
kRLEpisode      = 27
kRLAgentMessage = 33
kRLEnvMessage   = 34

kRLTerm         = 35

kLocalHost = "127.0.0.1"
kDefaultPort = "4096"
kRetryTimeout = 2 -- Currently unused

kDefaultBufferSize = 4096
kIntSize = 4
kDoubleSize = 8
kCharSize = 1

go :: ((Socket, SockAddr) -> IO r) -> IO r
go func =
  do
    host <- catchJust
      (\e -> if isDoesNotExistError e then Just () else Nothing)
      (getEnv "RLGLUE_HOST")
      (\_ -> return kLocalHost)
    port <- catchJust
      (\e -> if isDoesNotExistError e then Just () else Nothing)
      (getEnv "RLGLUE_PORT")
      (\_ -> return kDefaultPort)
    connect host port func
