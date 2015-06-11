{-# LANGUAGE ScopedTypeVariables #-}

module RLNetwork where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Word
import Network.Simple.TCP
import System.Environment
import System.IO.Error

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

glueConnect :: forall r. ((Socket, SockAddr) -> IO r) -> IO r
glueConnect func =
  do
    host <- catchJust
      (\e -> if isDoesNotExistError e then Just () else Nothing)
      (getEnv "RLGLUE_HOST")
      (\_ -> return kLocalHost)
    port <- catchJust
      (\e -> if isDoesNotExistError e then Just () else Nothing)
      (getEnv "RLGLUE_PORT")
      (\_ -> return kDefaultPort)
    let func' :: (Socket, SockAddr) -> IO r
        func' (sock, addr) = do
          putStrLn ("Connecting to " ++ (show addr) ++ " on port " ++ port ++ "...")
          func (sock, addr)
    connect host port func'

-- Send/Recv helper functions
doCallWithNoParams :: Socket -> Word32 -> IO ()
doCallWithNoParams sock x =
  do
    let bs = runPut (putWord32be x >> putWord32be (fromIntegral 0))
    sendLazy sock bs

doStandardRecv :: Socket -> MaybeT IO (Int, Int)
doStandardRecv sock =
  do
    bs <- MaybeT $ recv sock (2*4)
    return $ runGet parseBytes (LBS.fromStrict bs)
    where
      parseBytes = do
        glueState <- getWord32le
        dataSize <- getWord32le
        return (fromIntegral glueState, fromIntegral dataSize)

getString :: Socket -> MaybeT IO BS.ByteString
getString sock =
  do
    bs <- MaybeT $ recv sock (4)
    let length = fromIntegral $ runGet (getWord32le) (LBS.fromStrict bs)
    MaybeT $ recv sock (4*length)
