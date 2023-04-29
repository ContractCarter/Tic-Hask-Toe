{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module A2 where

import A1
import Data.List (intercalate)
import A1 (Player)

-- *** Assignment 2-1 *** --

-- Q#01

promptPlayer :: Player -> String
promptPlayer player = concat ["Player", show player,", enter a row and coloumn position:"]

-- Q#02

_SIZE_ :: Int
_SIZE_ = 3

_RANGE_ :: [Int]
_RANGE_ = [0..(_SIZE_ - 1)]


-- Q#03

isDigit :: Char -> Bool
isDigit c = elem c ['0'..'9']


readDigit :: Char -> Int
readDigit c
  | isDigit c = read [c]
  | otherwise = -1

-- Q#04

_EMPTY_ROW_ :: [Square]
_EMPTY_ROW_ = replicate _SIZE_ Empty

_EMPTY_BOARD_ :: Board
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05

isTied :: Board -> Bool
isTied board = all (notElem Empty) board

_TIED_BOARD_ :: Board
_TIED_BOARD_ = replicate _SIZE_ (replicate _SIZE_ X)
-- Q#06

indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings strings = zip ['A'..] strings
-- Q#07

formatLine :: [String] -> String
formatLine strList = _SEP_ ++ intercalate _SEP_ strList ++ _SEP_
-- *** Assignment 2-2 *** --

-- Q#08
type Move = (Int, Int)

_SIZE_ :: Int
_SIZE_ = 8 -- You can change this value to represent the size of your game board

isMoveInBounds :: Move -> Bool
isMoveInBounds (row, col) =
  let
    bounds = [0..(_SIZE_ - 1)]
    inBounds index = index `elem` bounds
  in
    all inBounds [row, col]
-- Q#09

stringToMove :: String -> Move
stringToMove [rowChar, colChar]
    | isDigit colChar = (convertRowIndex rowChar, readDigit colChar)
    | otherwise       = _INVALID_MOVE_
stringToMove _ = _INVALID_MOVE_
-- Q#10
data Player = X | O deriving (Show, Eq)
type Row = [Player]

replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow player columnIndex row
    | columnIndex < 0 || columnIndex >= length row = row
    | otherwise =
        let (firstPart, secondPart) = splitAt columnIndex row
        in firstPart ++ [player] ++ drop 1 secondPart

rsX :: Int -> Row -> Row
rsX = replaceSquareInRow X

rsO :: Int -> Row -> Row
rsO = replaceSquareInRow O