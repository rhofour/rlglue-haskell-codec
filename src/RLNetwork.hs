{-# LANGUAGE ScopedTypeVariables #-}

module RLNetwork where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Binary.Get
import Data.Binary.IEEE754
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
kExperimentConnection  = 1 :: Word32
kAgentConnection       = 2 :: Word32
kEnvironmentConnection = 3 :: Word32

kAgentInit      = 4 :: Word32
kAgentStart     = 5 :: Word32
kAgentStep      = 6 :: Word32
kAgentEnd       = 7 :: Word32
kAgentCleanup   = 8 :: Word32
kAgentMessage   = 10 :: Word32

kEnvInit        = 11 :: Word32
kEnvStart       = 12 :: Word32
kEnvStep        = 13 :: Word32
kEnvCleanup     = 14 :: Word32
kEnvMessage     = 19 :: Word32

kRLInit         = 20 :: Word32
kRLStart        = 21 :: Word32
kRLStep         = 22 :: Word32
kRLCleanup      = 23 :: Word32
kRLReturn       = 24 :: Word32
kRLNumSteps     = 25 :: Word32
kRLNumEpisodes  = 26 :: Word32
kRLEpisode      = 27 :: Word32
kRLAgentMessage = 33 :: Word32
kRLEnvMessage   = 34 :: Word32

kRLTerm         = 35 :: Word32

kLocalHost = "127.0.0.1"
kDefaultPort = "4096"
kRetryTimeout = 2 -- Currently unused

kDefaultBufferSize = 4096
kIntSize = 4
kDoubleSize = 8
kCharSize = 1

-- Datatypes
data RLAbstractType = RLAbstractType [Int] [Double] BS.ByteString
newtype Action = Action RLAbstractType
newtype Observation = Observation RLAbstractType
type Reward = Double
type Terminal = Int

-- Abstract type functions
sizeOfType :: RLAbstractType -> Int
sizeOfType (RLAbstractType ints doubles bs) =
  kIntSize * (3 + length ints) + kDoubleSize * (length doubles) + kCharSize * (BS.length bs)

sizeOfObs :: Observation -> Int
sizeOfObs (Observation absType) = sizeOfType absType

sizeOfRewardObs :: (Terminal, Reward, Observation) -> Int
sizeOfRewardObs (_, _, obs) =
  kIntSize + kDoubleSize + sizeOfObs obs

getAbstractType :: Socket -> MaybeT IO RLAbstractType
getAbstractType sock =
  do
    bs1 <- MaybeT $ recv sock (3*4)
    let (numInts, numDoubles, numChars) = runGet parseBytes1 (LBS.fromStrict bs1)
    bs2 <- MaybeT $ recv sock (numInts*4 + numDoubles*8 + numChars)
    return $ runGet (parseBytes2 numInts numDoubles numChars) (LBS.fromStrict bs2)
    where
      parseBytes1 = do
        numInts <- getWord32be
        numDoubles <- getWord32be
        numChars <- getWord32be
        return (fromIntegral numInts, fromIntegral numDoubles, fromIntegral numChars)
      parseBytes2 numInts numDoubles numChars = do
        ints <- replicateM (fromIntegral numInts) getWord32be
        doubles <- replicateM (fromIntegral numDoubles) getFloat64be
        chars <- getByteString (fromIntegral numChars)
        return (RLAbstractType (map fromIntegral ints) doubles chars)

putAbstractType :: RLAbstractType -> Put
putAbstractType (RLAbstractType ints doubles bs) = do
  let numInts = fromIntegral $ length ints
  let numDoubles = fromIntegral $ length doubles
  let numChars = fromIntegral $ BS.length bs
  putWord32be numInts
  putWord32be numDoubles
  putWord32be numChars
  mapM_ (putWord32be . fromIntegral) ints
  mapM_ putFloat64be doubles
  putByteString bs

putObservation :: Observation -> Put
putObservation (Observation absType) = putAbstractType absType

putTerminalRewardObs :: (Terminal, Reward, Observation) -> Put
putTerminalRewardObs (terminal, reward, Observation absType) = do
  putWord32be (fromIntegral terminal)
  putFloat64be reward
  putAbstractType absType

-- Actually connect
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
    let bs = runPut (putWord32be x >> putWord32be (0 :: Word32))
    sendLazy sock bs
    confirmState sock x

doStandardRecv :: Socket -> MaybeT IO (Word32, Word32)
doStandardRecv sock =
  do
    bs <- MaybeT $ recv sock (2*4)
    return $ runGet parseBytes (LBS.fromStrict bs)
    where
      parseBytes = do
        glueState <- getWord32be
        dataSize <- getWord32be
        return (glueState, dataSize)

getInt :: Socket -> MaybeT IO Int
getInt sock =
  do
    bs <- MaybeT $ recv sock 4
    return . fromIntegral $ runGet getWord32be (LBS.fromStrict bs)

getDouble :: Socket -> MaybeT IO Double
getDouble sock =
  do
    bs <- MaybeT $ recv sock 8
    return  $ runGet getFloat64be (LBS.fromStrict bs)

getString :: Socket -> MaybeT IO BS.ByteString
getString sock =
  do
    length <- getInt sock
    MaybeT $ recv sock (4*length)

putString :: BS.ByteString -> Put
putString bs = do
  putWord32be (fromIntegral (BS.length bs))
  putByteString bs

-- Other functions
confirmState :: Socket -> Word32 -> IO ()
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

sendMessage :: Word32 -> Socket -> BS.ByteString -> IO BS.ByteString
sendMessage selByte sock msg =
  do
    let 
      packedMsg = 
        runPut (
          putWord32be selByte >>
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
sendAgentMessage = sendMessage kRLAgentMessage

sendAgentMessageStr :: Socket -> String -> IO BS.ByteString
sendAgentMessageStr sock msg = sendAgentMessage sock (BSC.pack msg)

sendEnvMessage :: Socket -> BS.ByteString -> IO BS.ByteString
sendEnvMessage = sendMessage kRLEnvMessage

sendEnvMessageStr :: Socket -> String -> IO BS.ByteString
sendEnvMessageStr sock msg = sendEnvMessage sock (BSC.pack msg)
