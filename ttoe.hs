import Data.List
import Data.List.Split
import Data.Char
import System.IO

data Player = X | O
show_player :: Player -> String
show_player X = "☓"
show_player O = "○"
instance Show Player where
  show p = show_player p
instance Eq Player where
  (X) == (X) = True
  (O) == (O) = True
  _   == _   = False

data Cell = Occupied Player | Empty
show_cell :: Cell -> String
show_cell (Occupied p) = "[" ++ show p ++ "]"
show_cell Empty = "[_]"
instance Show Cell where
  show c = show_cell c
instance Eq Cell where
  (Occupied p1) == (Occupied p2) = p1 == p2
  Empty         == Empty         = True
  _             == _             = False

type Field = [Cell]
type Eval = (Int, Maybe Int)
type State = (Maybe Player, [Int])

show_field :: Field -> String
show_field field = intercalate "\n" (map unwords (chunksOf dim (map show field))) ++ "\n" where
  dim = dimension field

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

dimension :: Field -> Int
dimension field= (round.sqrt.fromIntegral.length) field

winning_combinations :: Field -> [[Cell]]
winning_combinations field = horizontals ++ verticals ++ diagonal1 ++ diagonal2 where
                             dim       = dimension field
                             horizontals = chunksOf dim field
                             verticals = transpose horizontals
                             diagonal1 = [(head field) : take_every_nth (dim + 1) (tail field)]
                             diagonal2 = [take dim (take_every_nth (dim - 1) (tail field))]

has_winner :: Field -> (Maybe Player)
has_winner field | winner X field = Just X
                 | winner O field = Just O
                 | otherwise = Nothing

possible_moves :: Field -> [Int]
possible_moves field = [i | (c,i) <- zip field [0..], c == Empty]

board_state :: Field -> State
board_state field = (has_winner field, possible_moves field)

game_over :: Field -> Bool
game_over field = case board_state field of
    (Just X, _) -> True
    (Just O, _) -> True
    (_, [])     -> True
    otherwise   -> False

opposite :: Player -> Player
opposite X = O
opposite O = X

minimax :: Field -> Player -> Int -> Eval
minimax field player depth = case board_state field of
  (Nothing, [])    -> (depth - 10, Nothing)
  (Just O, _)      -> (depth - 10, Nothing)
  (Just X, _)      -> (10 - depth, Nothing)
  (Nothing, moves) -> pick $ sort [(fst $ minimax (make_next_move n) opposite_player (depth + 1), Just n)
                                  | n <- moves] where
    pick = if player == O then head else last
    opposite_player = opposite player
    make_next_move = make_move player field

print_field :: Field -> IO ()
print_field field = do
  putStr "\n"
  putStr (show_field field)
  putStr "\n"

enter_move :: Field -> IO Int
enter_move field = do
  putStr "Enter move [0..8]: "
  move_str <- getLine
  let move = read move_str :: Int
  if (elem move (possible_moves field)) then
    return move
  else
    do enter_move field

cpu :: Field -> IO Field
cpu field = do
  let evl = minimax field O 0
  case snd evl of
    Just move -> return (make_move O field move)
    _         -> return field

play :: Field -> Player -> IO ()
play field player = do
  print_field field
  if game_over field then
    do putStr "Game over"
  else
    case player of
      X -> do
        move <- enter_move field;
        let field' = make_move X field move;
        play field' O;
      O -> do
        field' <- cpu field;
        play field' X;

main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  play (new_field 3) X
