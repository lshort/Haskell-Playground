-- This simple API is a solution to the problem posed at Tony
-- Morris's blog at http://tinyurl.com/4dp82ze

module Tictactoe ( Player, Position, Move, 
                   EmptyBoard, Board, CompleteBoard, Winner, 
                   whoseMoveIsIt, makeMove, undoMove, whoWonOrDraw, 
                   whoOccupiesPosition )
where

import Data.List

data Player = Player1 | Player2  deriving (Show, Eq)
data Position = NW | N | NE | W  | C | E  | SW | S | SE  
                deriving (Show, Eq)
data Winner = Draw | Winner Player
type Move = Position
winningPositions = [[NW,N,NE],[W,C,E],[SW,S,SE],
                    [NW,W,SW],[N,C,S],[NE,E,SE],
                    [NW,C,SE],[NE,C,SW]] :: [[Move]]

class MoveableBoard a where
   makeMove :: a -> Move -> Maybe (Either Board CompleteBoard)

class UndoableBoard a where
   undoMove :: a -> Either Board EmptyBoard


{-----------  Implemention: making and undoing moves ---------------}
data EmptyBoard = EmptyBoard deriving (Show)
instance MoveableBoard EmptyBoard where
  makeMove EmptyBoard x = Just  $ Left $ BoardCtor [x]

data Board = BoardCtor [Move] deriving (Show)
instance MoveableBoard Board where
  makeMove (BoardCtor xs) y 
     | y `elem` xs          = Nothing
     | completesBoard xs y  = Just $ Right $ CompleteBoardCtor $ y:xs
     | otherwise            = Just $ Left  $ BoardCtor $ y:xs
instance UndoableBoard Board where
  undoMove (BoardCtor [x])    = Right EmptyBoard
  undoMove (BoardCtor (x:xs)) = Left $ BoardCtor xs

data CompleteBoard = CompleteBoardCtor [Move]  deriving (Show)
instance UndoableBoard CompleteBoard where
  undoMove (CompleteBoardCtor (x:xs)) = Left (BoardCtor xs)

{-----------  Implemention: board queries ---------------}
whoseMoveIsIt :: Board -> Player
whoseMoveIsIt (BoardCtor xs) = whoseMove' xs

whoseMove' :: [Move] -> Player
whoseMove' xs = if (odd $ length xs) then Player2 else Player1 

whoWonOrDraw :: CompleteBoard -> Winner
whoWonOrDraw (CompleteBoardCtor ms)
  | winner Player1 ms = Winner Player1
  | winner Player2 ms = Winner Player2
  | otherwise         = Draw

whoOccupiesPosition :: Position -> Board -> Maybe Player
whoOccupiesPosition x (BoardCtor ys) = whoOccupiesPosition' x ys
    where whoOccupiesPosition' x [] = Nothing
          whoOccupiesPosition' x (y:ys)
              | x == y    = Just $ whoseMoveIsIt $ BoardCtor ys
              | otherwise = whoOccupiesPosition' x ys

completesBoard :: [Move] -> Move -> Bool
completesBoard xs y = length xs == 8 || 
                      winner Player1 (y:xs) || 
                      winner Player2 (y:xs)

winner :: Player -> [Move] -> Bool
winner p m = any ((flip subset) (getMoves m p)) winningPositions

subset :: Eq a => [a] -> [a] -> Bool
subset sub set = [] == sub \\ (intersect sub set)

getMoves :: [Move] -> Player -> [Move]
getMoves xs p = getMoves' xs (whoseMove' xs /= p)
   where getMoves' [] _ = []
         getMoves' (x:xs) True = x : getMoves' xs False
         getMoves' (x:xs) False = getMoves' xs True

{-----------  Some helper functions to ease GHCi repl debugging -------------}
addMove :: (Maybe (Either Board CompleteBoard)) -> Move -> (Maybe (Either Board CompleteBoard))
addMove (Just (Left x)) y  = makeMove x y

takeBack :: (Maybe (Either Board CompleteBoard)) -> (Either Board EmptyBoard)
takeBack (Just (Left x)) = undoMove x

getBoardFromBlob :: (Maybe (Either Board a)) -> Board
getBoardFromBlob x = either id (\x -> BoardCtor []) $ maybe (Left (BoardCtor [])) id x
