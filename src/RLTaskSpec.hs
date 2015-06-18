module RLTaskSpec where

import qualified Data.ByteString as BS
import Text.Parsec
import Text.Parsec.ByteString

data TaskSpec = TaskSpec ProblemType
  deriving (Show)
data ProblemType = Episodic | Continuing | OtherProblemType String
  deriving (Show)

toTaskSpec :: BS.ByteString -> Either ParseError TaskSpec
toTaskSpec = parse parseTaskSpec "(network)"

parseTaskSpec = do
  parseVersion
  spaces
  probType <- parseProblemType
  return $ TaskSpec probType

parseVersion = do
  -- We currently only parse version 3.0
  string "VERSION"
  spaces
  string "RL-Glue-3.0"

parseProblemType = do
  string "PROBLEMTYPE"
  spaces
  probTypeStr <- many (letter <|> digit)
  return $ case probTypeStr of
    "episodic" -> Episodic
    "continuing" -> Continuing
    _ -> OtherProblemType probTypeStr
