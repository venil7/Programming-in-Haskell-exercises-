module Main where

import Data.Array
import Data.Maybe
import Data.String.Common
import Data.Tuple
import Prelude

import Effect (Effect)
import Effect.Console (error, log)
import Utils (chunks)

data Piece = Pawn
  | Tower
  | Knight
  | Bishop
  | Queen
  | King
derive instance eqPiece :: Eq Piece

data Cell = Empty | Occ Player Piece
derive instance eqCell :: Eq Cell

data Player = Black | White
derive instance eqPlayer :: Eq Player

instance showCell :: Show Cell where
  show (Occ White Knight) = "♟"
  show (Occ White Knight) = "♞"
  show (Occ White Tower)  = "♜"
  show (Occ White Bishop) = "♝"
  show (Occ White Queen)  = "♛"
  show (Occ White King)   = "♚"
  show (Occ Black Pawn)   = "♙"
  show (Occ Black Knight) = "♘"
  show (Occ Black Tower)  = "♖"
  show (Occ Black Bishop) = "♗"
  show (Occ Black Queen)  = "♕"
  show (Occ Black King)   = "♔"
  show _ = " "

black :: Piece -> Cell
black = Occ Black

white :: Piece -> Cell
white = Occ White

newtype Board = Board (Array Cell)
newtype Move = Move (Tuple Int Int)

fromTo :: Int -> Int -> Move
fromTo from to = Move (Tuple from to)

board :: Board
board = Board $ [
  (black Tower), (black Knight), (black Bishop), (black Queen), (black King), (black Bishop), (black Knight), (black Tower),
  Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
  Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
  Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
  Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
  Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
  (white Tower), (white Knight), (white Bishop), (white Queen), (white King), (white Bishop), (white Knight), (white Tower)
]

instance showBoard :: Show Board where
  show (Board cells) = joinWith "\n" strRows where
    rows = chunks 8 cells
    rowToStr = \row -> joinWith "|" (map show row)
    strRows = map rowToStr rows

move :: Board -> Move -> Board
move (Board cells) (Move coord) = Board $ updateAtIndices [Tuple from Empty, Tuple to piece] cells where
  from = fst coord
  to = snd coord
  piece = fromMaybe Empty (cells !! from)

at :: Board -> Int -> Cell
at (Board cells) idx = fromMaybe Empty (cells !! idx)

main :: Effect Unit
main = do
  log $ show board
