module Utils where

import Data.Array (drop, head, tail, take, uncons)
import Data.Boolean (otherwise)
import Data.Foldable (class Foldable, null)
import Data.Maybe
import Prelude (pure, ($), (<>))

chunks :: forall a. Int -> Array a -> Array (Array a)
chunks _ [] = []
chunks n xs = pure (take n xs) <> (chunks n $ drop n xs)

join :: forall a. (a -> String) -> Array a -> String
join f arr = case uncons arr of
  Just { head: x, tail: xs } -> f x <> join f xs
  Nothing -> ""