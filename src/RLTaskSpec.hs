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

data ObservationType = ObservationType IntObsType DoubleObsType CharObsType
  deriving (Show)
type IntObsType = [ObsBounds Int]
type DoubleObsType = [ObsBounds Double]
type CharObsType = Int
type ObsBounds a = (LowBound a, UpBound a)
data LowBound a = LowBound a | NegInf | LBUnspec
  deriving (Show)
data UpBound a = UpBound a | PosInf | UBUnspec
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
  doubleObsType <- parseDoubleObsType
  charObsType <- parseCharObsType
  return $ ObservationType intObsType doubleObsType charObsType

parseRepeatable :: Parsec BS.ByteString () a -> Parsec BS.ByteString () [a]
parseRepeatable parser = do
  try (do
    times <- liftM read $ many1 digit
    spaces
    x <- parser
    return $ replicate times x)
  <|> liftM (\x -> [x]) parser

parseIntObsType :: Parsec BS.ByteString () IntObsType
parseIntObsType = (try $ do
  string "INTS"
  spaces
  xs <- many (do
    x <- parseObsTypeTuple
    spaces
    return x)
  return $ concat xs) <|> return []

parseDoubleObsType :: Parsec BS.ByteString () DoubleObsType
parseDoubleObsType = (try $ do
  string "DOUBLES"
  spaces
  xs <- many (do
    x <- parseObsTypeTuple
    spaces
    return x)
  return $ concat xs) <|> return []

parseObsTypeTuple :: Read a => Parsec BS.ByteString () [ObsBounds a]
parseObsTypeTuple = do
  char '('
  x <- parseRepeatable parseBoundsTuple
  char ')'
  return x

parseBoundsTuple :: Read a => Parsec BS.ByteString () (ObsBounds a)
parseBoundsTuple = do
  lb <- parseLB
  spaces
  ub <- parseUB
  return (lb, ub)

parseLB :: Read a => Parsec BS.ByteString () (LowBound a)
parseLB = 
  (liftM (LowBound . read) $ many1 digit) <|> 
  (string "NEGINF" >> return NegInf) <|>
  (string "UNSPEC" >> return LBUnspec)

parseUB :: Read a => Parsec BS.ByteString () (UpBound a)
parseUB = 
  (liftM (UpBound . read) $ many1 digit) <|> 
  (string "POSINF" >> return PosInf) <|>
  (string "UNSPEC" >> return UBUnspec)

parseCharObsType :: Parsec BS.ByteString () CharObsType
parseCharObsType = (try $ do
  string "CHARCOUNT"
  spaces
  liftM read (many1 digit)) <|> return 0
