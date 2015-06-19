module RLTaskSpec where

import Control.Monad
import qualified Data.ByteString as BS
import Text.Parsec
import Text.Parsec.ByteString

-- Datatype definitions
data TaskSpec = TaskSpec ProblemType DiscountFactor ObservationType
  deriving (Show)
data ProblemType = Episodic | Continuing | OtherProblemType String
  deriving (Show)
type DiscountFactor = Double
data ObservationType = ObservationType IntObsType
  deriving (Show)
type IntObsType = [IntObsBounds]
type IntObsBounds = (IntLowBound, IntUpBound)
data IntLowBound = IntLowBound Int | ILB_NegInf | ILB_Unspec
  deriving (Show)
data IntUpBound = IntUpBound Int | IUB_PosInf | IUB_Unspec
  deriving (Show)

-- Parsing functions
toTaskSpec :: BS.ByteString -> Either ParseError TaskSpec
toTaskSpec = parse parseTaskSpec "(network)"

parseTaskSpec = do
  parseVersion
  spaces
  probType <- parseProblemType
  spaces
  discountFactor <- parseDiscountFactor
  spaces
  obsType <- parseObservationType
  return $ TaskSpec probType discountFactor obsType

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

parseDiscountFactor = do
  string "DISCOUNTFACTOR"
  spaces
  numStr <- many1 (digit <|> char '.')
  return $ read numStr

parseObservationType :: Parsec BS.ByteString () ObservationType
parseObservationType = do
  string "OBSERVATIONS"
  spaces
  intObsType <- parseIntObsType
  return $ ObservationType intObsType

parseRepeatable :: Parsec BS.ByteString () a -> Parsec BS.ByteString () [a]
parseRepeatable parser = do
  try (do
    times <- liftM read $ many1 digit
    spaces
    x <- parser
    return $ replicate times x)
  <|> liftM (\x -> [x]) parser

parseIntObsType :: Parsec BS.ByteString () IntObsType
parseIntObsType = do
  string "INTS"
  spaces
  xs <- many (do
    x <- parseIntObsTypeTuple
    spaces
    return x)
  return $ concat xs

parseIntObsTypeTuple :: Parsec BS.ByteString () IntObsType
parseIntObsTypeTuple = do
  char '('
  x <- parseRepeatable parseIntBoundsTuple
  char ')'
  return x

parseIntBoundsTuple :: Parsec BS.ByteString () IntObsBounds
parseIntBoundsTuple = do
  lb <- parseIntLB
  spaces
  ub <- parseIntUB
  return (lb, ub)

parseIntLB :: Parsec BS.ByteString () IntLowBound
parseIntLB = 
  (liftM (IntLowBound . read) $ many1 digit) <|> 
  (string "NEGINF" >> return ILB_NegInf) <|>
  (string "UNSPEC" >> return ILB_Unspec)

parseIntUB :: Parsec BS.ByteString () IntUpBound
parseIntUB = 
  (liftM (IntUpBound . read) $ many1 digit) <|> 
  (string "POSINF" >> return IUB_PosInf) <|>
  (string "UNSPEC" >> return IUB_Unspec)
