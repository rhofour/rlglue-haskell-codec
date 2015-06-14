module RLEnvironment where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import Data.Version (showVersion)
import Data.Word
import Network.Simple.TCP
import System.Exit

import Paths_rlglue_haskell_codec (version)
import RLNetwork

data Environment a = Environment
  { onEnvInit :: (StateT a IO ())
  , onEnvStart :: (StateT a IO ())
  , onEnvStep :: (StateT a IO ())
  , onEnvCleanup :: (StateT a IO ())
  , onEnvMessage :: (StateT a IO ())
  }

loadEnvironment ::  Environment a -> a -> IO ()
loadEnvironment env initState =
  let 
    func (sock, addr) =
      do
        -- Initial setup
        putStrLn ("RL-Glue Haskell Environment Codec (Version " ++ (showVersion version) ++ ")")
        let bs = runPut (putWord32be kEnvironmentConnection >> putWord32be (0 :: Word32))
        sendLazy sock bs

        -- Run event loop
        evalStateT (eventLoop env sock) initState
  in
    glueConnect func

eventLoop :: Environment a -> Socket -> StateT a IO ()
eventLoop env sock = do
  x <- lift $ runMaybeT (getEnvState sock)
  case x of
    Nothing -> do
      lift $ putStrLn "Error: Failed to receive state."
      lift $ exitWith (ExitFailure 1)
    Just (state, size) -> case state of
      kEnvInit -> onEnvInit env
      kEnvStart -> onEnvStart env
      kEnvStep -> onEnvStep env
      kEnvCleanup -> onEnvCleanup env
      kEnvMessage -> onEnvMessage env
      kRLTerm -> lift $ return ()
      _ -> do
        lift $ putStrLn $ "Error: Unknown state: " ++ (show state)
        lift $ exitWith (ExitFailure 1)

getEnvState :: Socket -> MaybeT IO (Word32, Word32)
getEnvState sock = do
  bs <- MaybeT $ recv sock (4*2)
  return $ runGet (parseBytes) (LBS.fromStrict bs)
  where
    parseBytes = do
      envState <- getWord32be
      dataSize <- getWord32be
      return (envState, dataSize)

loopUntil :: IO Bool -> IO ()
loopUntil f = do
  x <- f
  if x then return () else loopUntil f
