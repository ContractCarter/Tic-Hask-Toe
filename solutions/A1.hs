module A1 where

import Data.Char (toUpper)
import Text.ParserCombinators.ReadP (char)

-- *** Assignment 1-1 *** --

-- Q#01

_SIZE_ :: Int
_SIZE_ = 3
-- Q#02

_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True
-- Q#03

convertRowIndex :: Char -> Int
convertRowIndex index = fromEnum (toUpper index) - 65\
-- Q#04
_INVALID_MOVE_ :: (Int, Int)
_INVALID_MOVE_ = (-1, -1)

-- Q#05
_SEP_ :: String 
_SEP_ = "_|_"

-- *** Assignment 1-2 *** --

-- Q#06
data Square = X | O | Empty deriving (Show, Eq)


-- Q#07
data GameState = XWin | OWin | Tie | InProgress deriving (Show, Eq)


-- Q#08
type Player = Square 
type Row   = [Square]
type Line  = [Square]
type Board = [Row]
type Move  = (Int, Int)



-- Q#09
-- if else
getFirstPlayer :: Bool -> Player
getFirstPlayer isTrue = if isTrue then X else O

-- guards
getFirstPlayer_ :: Bool -> Player
getFirstPlayer_ isTrue
  | isTrue = X
  | otherwise = O

-- Q#10

showGameState :: GameState -> String
showGameState gameState = case gameState of
  XWin -> "X wins the game!"
  OWin -> "O wins the game!"
  Tie -> "The game is a tie."
  InProgress -> "The game is in progress."


-- Q#11

switchPlayer :: Player -> Player
switchPlayer X = 0
switchPlayer 0 = X
switchPlayer Empty = Empty

-- Q#12

showSquare :: Square -> String
showSquare X = "X"
showSquare O = "O"
showSquare Empty = "_"