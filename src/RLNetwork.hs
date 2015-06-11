{-# LANGUAGE ScopedTypeVariables #-}

module RLNetwork where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.Word
import Network.Simple.TCP
import System.Environment
import System.Exit
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
          x <- func (sock, addr)
          putStrLn ("Disconnecting from " ++ (show addr) ++ " on port " ++ port ++ "...")
          return x
    connect host port func'

-- Send/Recv helper functions
doCallWithNoParams :: Socket -> Word32 -> IO ()
doCallWithNoParams sock x =
  do
    let bs = runPut (putWord32be x >> putWord32be (fromIntegral 0))
    sendLazy sock bs

doStandardRecv :: Socket -> MaybeT IO (Integer, Integer)
doStandardRecv sock =
  do
    bs <- MaybeT $ recv sock (2*4)
    return $ runGet parseBytes (LBS.fromStrict bs)
    where
      parseBytes = do
        glueState <- getWord32be
        dataSize <- getWord32be
        return (fromIntegral glueState, fromIntegral dataSize)

getString :: Socket -> MaybeT IO BS.ByteString
getString sock =
  do
    bs <- MaybeT $ recv sock (4)
    let length = fromIntegral $ runGet (getWord32be) (LBS.fromStrict bs)
    MaybeT $ recv sock (4*length)

-- Other functions
confirmState :: Socket -> Integer -> IO ()
confirmState sock exptState =
  do
    x <- runMaybeT (doStandardRecv sock)
    case x of
      Nothing -> do
        putStrLn "Failed to receive state. Exiting..."
        exitWith (ExitFailure 1)
      Just (state, size) -> if state == exptState then return () else do
        putStrLn $ "State " ++ (show state) ++ " doesn't match expected state " ++
          (show exptState) ++ ". Exiting..."
        exitWith (ExitFailure 1)

sendMessage :: Integer -> Socket -> BS.ByteString -> IO BS.ByteString
sendMessage selByte sock msg =
  do
    let 
      packedMsg = 
        runPut (
          putWord32be (fromIntegral selByte) >>
          putWord32be (fromIntegral (4 + BS.length msg)) >>
          putWord32be (fromIntegral (BS.length msg)) >>
          putByteString msg)
    sendLazy sock packedMsg
    confirmState sock selByte 
    resp <- runMaybeT (getString sock)
    case resp of
      Nothing -> do
        putStrLn "Error: Could not read response from agent message"
        exitWith (ExitFailure 1)
      Just x -> return x

sendAgentMessage :: Socket -> BS.ByteString -> IO BS.ByteString
sendAgentMessage = sendMessage (fromIntegral kRLAgentMessage)

sendAgentMessageStr :: Socket -> String -> IO BS.ByteString
sendAgentMessageStr sock msg = sendAgentMessage sock (BSC.pack msg)

sendEnvMessage :: Socket -> BS.ByteString -> IO BS.ByteString
sendEnvMessage = sendMessage (fromIntegral kRLEnvMessage)

sendEnvMessageStr :: Socket -> String -> IO BS.ByteString
sendEnvMessageStr sock msg = sendEnvMessage sock (BSC.pack msg)
