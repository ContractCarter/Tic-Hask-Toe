module A3 where

import A1
import A2

import Data.List (transpose)

-- *** Assignment 3-1 ***

-- Q#01

showInts :: [Int] -> [String]
showInts [] = []
showInts (x:xs) = show x : showInts xs

_RANGE_ :: [Int]
_RANGE_ = [1..10]

formatLine :: [String] -> String
formatLine [] = ""
formatLine [x] = x
formatLine (x:xs) = x ++ " | " ++ formatLine xs

_HEADER_ :: String
_HEADER_ = ' ' : formatLine (showInts _RANGE_)

_HEADER_ = undefined

-- Q#02

showSquares :: [Square] -> [String]
showSquares [] = []
showSquares (x:xs) = showSquare x : showSquares xs


-- Q#03

formatRows :: [Row] -> [String]
formatRows [] = []
formatRows (r:rs) = formatLine (showSquares r) : formatRows rs

-- Q#04

isColEmpty :: Row -> Int -> Bool
isColEmpty [] _ = False
isColEmpty (x:xs) 0 = x == Empty
isColEmpty (_:xs) n = isColEmpty xs (n - 1)

-- Q#05

dropFirstCol :: Board -> Board
dropFirstCol [] = []
dropFirstCol (r:rs) = tail r : dropFirstCol rs

dropLastCol :: Board -> Board
dropLastCol [] = []
dropLastCol (r:rs) = init r : dropLastCol rs


-- Q#06
getDiag1 :: Board -> Line
getDiag1 [] = []
getDiag1 (r:rs) = head r : getDiag1 (dropFirstCol rs)

getDiag2 :: Board -> Line
getDiag2 [] = []
getDiag2 (r:rs) = last r : getDiag2 (dropLastCol rs)


getAllLines :: Board -> [Line]
getAllLines board = concat [horizontalLines, verticalLines, diagonalLines]
  where
    horizontalLines = board
    verticalLines = transpose board
    diagonalLines = [getDiag1 board, getDiag2 board]

-- *** Assignment 3-2 ***

-- Q#07

putSquare :: Player -> Board -> Move -> Board
putSquare _ [] _ = []
putSquare player (row:rows) (moveRow, col)
    | moveRow == 0 = replaceSquareInRow row col player : rows
    | otherwise = row : putSquare player rows (moveRow - 1, col)

replaceSquareInRow :: [Char] -> Int -> Char -> [Char]
replaceSquareInRow row col player = 
    let (before, _:after) = splitAt col row
    in before ++ (player:after)
-- Q#08

type RowStrings = [String]

prependRowIndices :: RowStrings -> RowStrings
prependRowIndices rowStrings = worker (indexRowStrings rowStrings)
  where
    worker :: [(Char, String)] -> RowStrings
    worker [] = []
    worker ((index, row):rest) = (index : row) : worker rest

indexRowStrings :: RowStrings -> [(Char, String)]
indexRowStrings rowStrings = zip ['A'..] rowStrings

-- Q#09


isWinningLine :: Player -> Line -> Bool
isWinningLine player line = worker False line
  where
    worker :: Bool -> Line -> Bool
    worker acc [] = acc
    worker acc (square:rest)
      | square /= player = False
      | otherwise = worker True rest

-- Q#10

isValidMove :: Board -> Move -> Bool
isValidMove board move
  | not (isMoveInBounds board move) = False
  | otherwise = worker board move
  where
    worker :: Board -> Move -> Bool
    worker [] _ = False
    worker (row:rows) (moveRow, col)
      | moveRow == 0 = isColEmpty row col
      | otherwise = worker rows (moveRow - 1, col)

isMoveInBounds :: Board -> Move -> Bool
isMoveInBounds board (row, col) =
  row >= 0 && row < length board && col >= 0 && col < length (head board)

isColEmpty :: [Char] -> Int -> Bool
isColEmpty row col = row !! col == '-'