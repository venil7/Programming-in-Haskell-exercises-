module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Main
import Test.QuickCheck (quickCheck)
import Type.Data.Boolean (kind Boolean)
import Data.Tuple

moveActuallyMoves :: Int -> Int -> Boolean
moveActuallyMoves from to = fromPiece == toPiece && emptyCell == Empty where
  pieceMove = fromTo from to
  origBoard = board
  modBoard = move origBoard pieceMove
  fromPiece = at origBoard from
  emptyCell = at modBoard from
  toPiece = at modBoard to

coordCorrect :: Boolean
coordCorrect = coord 54 == Tuple 6 6

uncoordCorrect :: Boolean
uncoordCorrect = uncoord (Tuple 6 6) == 54

main :: Effect Unit
main = do
  quickCheck coordCorrect
  quickCheck uncoordCorrect
  quickCheck moveActuallyMoves
