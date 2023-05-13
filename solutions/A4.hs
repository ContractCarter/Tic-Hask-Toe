module A4 where

import A1
import A2
import A3 hiding (
  _HEADER_,
  showSquares,
  dropFirstCol,
  dropLastCol,
  formatRows,
  isWinningLine,
  prependRowIndices
  )

-- *** Assignment 4-1 *** --

-- Q#01

_HEADER_ :: String
_HEADER_  =  " " ++ formatLine (map show _RANGE_)
-- Q#02
showSquares :: [Square] -> [String]
showSquares xs = map show xs
-- Q#03

dropFirstCol :: Board -> Board
dropFirstCol str = map tail str

-- Q#04

dropLastCol :: Board -> Board
dropLastCol str = map init str
--Q#05

formatRows :: [Row] -> [String]
formatRows str =  map  (\x -> formatLine (showSquares x)) str
-- Q#06

isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ _ [] = False
isWinningLine_ p line = null (filter (\x -> p/=x) line)


-- *** Assignment 4-2 *** --

-- Q#07

isWinningLine :: Player -> Line -> Bool
isWinningLine p line = foldr (\lineb acc  -> acc && (p==lineb)) True line
-- Q#08

hasWon :: Player -> Board -> Bool
hasWon p brd = foldr (\linex acc -> acc || isWinningLine p linex) False (getAllLines brd)

_X_WIN_ = [ [X, O, O]
          , [O, X, O]
          , [O, O, X]
          ]

_O_WIN_ = [ [O, X, O]
          , [X, X, O]
          , [X, O, O]
          ]

-- Q#09

getGameState :: Board -> GameState
getGameState brd
  | hasWon X brd = X_won
  | hasWon O brd = O_won
  | isTied brd = Tie
  | otherwise = In_progress

playMove :: Player -> Board -> Move -> (GameState, Board)
playMove player board move = (gameState, newBoard)
    where
        newBoard = putSquare player board move
        gameState = getGameState newBoard
-- Q#10

prependRowIndices :: [String] -> [String]
prependRowIndices a = zipWith (++) azlist a
  where azlist = map (:[]) ['A'..'Z']
-- Q#11

formatBoard :: Board -> String 
formatBoard brd = unlines $ _HEADER_ : ( prependRowIndices $  formatRows brd)