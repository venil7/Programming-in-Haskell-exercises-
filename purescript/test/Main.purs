module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Main
import Test.QuickCheck (quickCheck)
import Type.Data.Boolean (kind Boolean)

moveActuallyMoves :: Int -> Int -> Boolean
moveActuallyMoves from to = fromPiece == toPiece && emptyCell == Empty where
  pieceMove = fromTo from to
  origBoard = board
  modBoard = move origBoard pieceMove
  fromPiece = at origBoard from
  emptyCell = at modBoard from
  toPiece = at modBoard to

main :: Effect Unit
main = do
  quickCheck moveActuallyMoves
