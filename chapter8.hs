-- 1
data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n        = Zero
mult (Succ Zero) n = n
mult (Succ m) n    = add n (mult m n)

add' :: Int -> Int -> Int
add' m n = m + n

mult' :: Int -> Int -> Int
mult' 0 n = 0
mult' 1 n = n
mult' m n = add' n (mult' (m-1) n)

-- 2
data Tree a = Leaf a | Node (Tree a) a (Tree a)
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y
occurs' x (Node l y r) = case compare x y of
                          EQ -> True
                          LT -> occurs' x l
                          GT -> occurs' x r

-- 3
num_leaves :: Tree a -> Int
num_leaves (Leaf a)   = 1
num_leaves (Node l _ r) = num_leaves l + num_leaves r

balanced :: Tree a -> Bool
balanced (Leaf a)   = True
balanced (Node l _ r) = abs ((num_leaves l) - (num_leaves r)) <= 1

-- 4
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving (Show)
halve :: [a] -> ([a], [a])
halve xs = (take half_length xs, drop half_length xs) where
            half_length = length xs `div` 2

balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs = Node' (balance half1) (balance half2) where
             (half1, half2) = halve xs

-- 5
data Expr = Val Int | Add Expr Expr deriving (Show)
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val i)   = f i
folde f g (Add l r) = g (folde f g l) (folde f g r)

-- 6
-- let expr = Add (Add (Val 1) (Val 4)) (Add (Val 3) (Val 5))
eval :: Expr -> Int
eval expr = folde id (+) expr

size :: Expr -> Int
size (Val _) = 1
size (Add l r) = (size l) + (size r)

-- 7
instance Eq a => Eq (Maybe a) where
  (Just x) == (Just y) = x == y
  _        == _        = False

instance Eq a => Eq [a] where
  []     == []     = False
  _      == ys     = False
  xs     == _      = False
  (x:xs) == (y:ys) = xs == ys