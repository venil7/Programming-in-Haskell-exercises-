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