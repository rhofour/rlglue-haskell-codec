module RLTaskSpec where

import Control.Monad
import qualified Data.ByteString as BS
import Text.Parsec
import Text.Parsec.ByteString

-- Datatype definitions
data TaskSpec = TaskSpec ProblemType DiscountFactor AbsDataType AbsDataType RewardBounds String
  deriving (Show)

data ProblemType = Episodic | Continuing | OtherProblemType String
  deriving (Show)

type DiscountFactor = Double

data AbsDataType = AbsDataType IntsBounds DoublesBounds NumChars
  deriving (Show)
type IntsBounds = [DataBounds Int]
type DoublesBounds = [DataBounds Double]
type NumChars = Int
type DataBounds a = (LowBound a, UpBound a)
data LowBound a = LowBound a | NegInf | LBUnspec
  deriving (Show)
data UpBound a = UpBound a | PosInf | UBUnspec
  deriving (Show)

type RewardBounds = DataBounds Double

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
  obs <- parseObservations
  spaces
  act <- parseActions
  spaces
  reward <- parseRewards
  spaces
  extra <- parseExtra
  return $ TaskSpec probType discountFactor obs act reward extra

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

parseObservations = do
  string "OBSERVATIONS"
  spaces
  parseAbsDataType

parseActions = do
  string "ACTIONS"
  spaces
  parseAbsDataType

parseRewards = do
  string "REWARDS"
  spaces
  char '('
  lower <- parseLB
  spaces
  upper <- parseUB
  char ')'
  return (lower, upper)

parseExtra = do
  string "EXTRA"
  spaces
  many anyChar

parseAbsDataType :: Parsec BS.ByteString () AbsDataType
parseAbsDataType = do
  intObsType <- parseIntsBounds
  doubleObsType <- parseDoublesBounds
  charObsType <- parseNumChars
  return $ AbsDataType intObsType doubleObsType charObsType

parseRepeatable :: Parsec BS.ByteString () a -> Parsec BS.ByteString () [a]
parseRepeatable parser =
  try (do
    times <- liftM read $ many1 digit
    spaces
    x <- parser
    return $ replicate times x)
  <|> liftM (: []) parser

parseIntsBounds :: Parsec BS.ByteString () IntsBounds
parseIntsBounds = try (do
  string "INTS"
  parseInnerBounds) <|> return []

parseDoublesBounds :: Parsec BS.ByteString () DoublesBounds
parseDoublesBounds = try (do
  string "DOUBLES"
  parseInnerBounds) <|> return []

parseInnerBounds :: Read a => Parsec BS.ByteString () [DataBounds a]
parseInnerBounds = do
  spaces
  xs <- many (do
    x <- parseAbsDataTypeTuple
    spaces
    return x)
  return $ concat xs

parseAbsDataTypeTuple :: Read a => Parsec BS.ByteString () [DataBounds a]
parseAbsDataTypeTuple = do
  char '('
  x <- parseRepeatable parseBoundsTuple
  char ')'
  return x

parseBoundsTuple :: Read a => Parsec BS.ByteString () (DataBounds a)
parseBoundsTuple = do
  lb <- parseLB
  spaces
  ub <- parseUB
  return (lb, ub)

parseLB :: Read a => Parsec BS.ByteString () (LowBound a)
parseLB = 
  liftM (LowBound . read) (many1 $ char '-' <|> char '.' <|> digit) <|> 
  (string "NEGINF" >> return NegInf) <|>
  (string "UNSPEC" >> return LBUnspec)

parseUB :: Read a => Parsec BS.ByteString () (UpBound a)
parseUB = 
  liftM (UpBound . read) (many1 $ char '-' <|> char '.' <|> digit) <|> 
  (string "POSINF" >> return PosInf) <|>
  (string "UNSPEC" >> return UBUnspec)

parseNumChars :: Parsec BS.ByteString () NumChars
parseNumChars = try (do
  string "CHARCOUNT"
  spaces
  liftM read (many1 digit)) <|> return 0
