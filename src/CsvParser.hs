{-# OPTIONS_GHC -Wno-missing-methods #-}

module CsvParser (fromString, Csv (..), Header, CsvLine) where

type CsvLine = [String]

type Header = Maybe CsvLine

data Csv = Csv Header [CsvLine] deriving (Show, Eq)

fromString :: Bool -> String -> Either String Csv
fromString _ [] = Right $ Csv Nothing []
fromString False input = Right $ Csv Nothing (toLines input)
fromString True input = case lines input of
  [] -> Left "Failed to parse csv with header: no lines found"
  [line] -> Right $ Csv (Just $ toLine line) []
  (x : xs) -> Right $ Csv (Just $ toLine x) $ map toLine xs

toLines :: String -> [CsvLine]
toLines = map toLine . lines

toLine :: String -> CsvLine
toLine = doToLine Unquoted ""

data QuoteState = Quoted | Unquoted deriving (Enum, Bounded, Eq)

-- | Bounded succ
succB :: (Enum a, Bounded a, Eq a) => a -> a
succB x
  | maxBound == x = minBound
  | otherwise = succ x

doToLine :: QuoteState -> String -> String -> CsvLine
doToLine _ [] [] = []
doToLine quotestate word [] = reverse word : doToLine quotestate [] []
doToLine Unquoted word (',' : xs) = reverse word : doToLine Unquoted [] xs
doToLine quotestate word ('"' : xs) = doToLine (succB quotestate) ('"' : word) xs
doToLine quotestate word (x : xs) = doToLine quotestate (x : word) xs
