import Data.List
import Data.List.Split

data Player = X | O
showPlayer :: Player -> String
showPlayer X = "☓"
showPlayer O = "○"
instance Show Player where
  show p = showPlayer p
instance Eq Player where
  (X) == (X) = True
  (O) == (O) = True
  _   == _   = False

data Cell = Occupied Player | Empty
showCell :: Cell -> String
showCell (Occupied p) = "[" ++ show p ++ "]"
showCell Empty = "_"
instance Show Cell where
  show c = showCell c
instance Eq Cell where
  (Occupied p1) == (Occupied p2) = p1 == p2
  Empty         == Empty         = True
  _             == _             = False

type Field = [Cell]
type Eval = (Int, Maybe Int)
type State = (Maybe Player, [Int])

new_field :: Int -> Field
new_field side | side > 0 = [Empty | _ <- [1..side^2]]
               | otherwise = error "can't use negative side size"

make_move :: Player -> Field -> Int -> Field
make_move player field n | (n >= length field) = error "can't set field beyond index"
                         | ((field !! n) /= Empty) = error "can't set field already occupied"
                         | otherwise = take n field ++ [(Occupied player)] ++ drop (n + 1) field

occupied_cell :: Player -> Cell -> Bool
occupied_cell player Empty = False
occupied_cell player (Occupied p) = player == p

occupied_line :: Player -> [Cell] -> Bool
occupied_line p cells = all (occupied_cell p) cells

winner :: Player -> Field -> Bool
winner player field = any (occupied_line player) (winning_combinations field)

take_every_nth :: Int -> [a] -> [a]
take_every_nth n = map snd . filter ((== n) . fst) . zip (cycle [1..n])

winning_combinations :: Field -> [[Cell]]
winning_combinations field = horizontals ++ verticals ++ diagonal1 ++ diagonal2 where
                             dimension = (round.sqrt.fromIntegral.length) field
                             horizontals = chunksOf dimension field
                             verticals = transpose horizontals
                             diagonal1 = [(head field) : take_every_nth (dimension + 1) (tail field)]
                             diagonal2 = [take dimension (take_every_nth (dimension - 1) (tail field))]

has_winner :: Field -> (Maybe Player)
has_winner field | winner X field = Just X
                 | winner O field = Just O
                 | otherwise = Nothing

possible_moves :: Field -> [Int]
possible_moves field = [i | (c,i) <- zip field [0..], c == Empty]

board_state :: Field -> State
board_state field = (has_winner field, possible_moves field)

opposite :: Player -> Player
opposite X = O
opposite O = X

minimax :: Field -> Player -> Int -> Eval
minimax field player depth = case board_state field of
  (Nothing, [])    -> (depth - 10, Nothing)
  (Just O, _)      -> (depth - 10, Nothing)
  (Just X, _)      -> (10 - depth, Nothing)
  (Nothing, moves) -> pick $ sort [(fst $ minimax (make_next_move n) opposite_player (depth + 1), Just n) | n <- moves] where
    pick = if player == O then head else last
    opposite_player = opposite player
    make_next_move = make_move player field
