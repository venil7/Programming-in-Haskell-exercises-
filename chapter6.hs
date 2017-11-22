-- 1
factorial :: Int -> Int
factorial n
          | n == 0 = 1
          | n < 0 = error "negative"
          | otherwise = n * factorial (n-1)
-- 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- 3
(%) :: Int -> Int -> Int
m % 0 = 1
m % 1 = m
m % n = m * (m % (n - 1))

-- 4
euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | x > y = euclid y (x - y)
           | x < y = euclid x (y - x)

-- 5
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' 1 (x:xs) = xs
drop' n (x:xs) = drop' (n-1) xs

init' :: [a] -> [a]
init' [] = []
init' [x] = []
init' (x:xs) = x : init' xs

-- 6
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

(!?) :: [a] -> Int -> a
(x:xs) !? 0 = x
(x:xs) !? n = xs !! (n - 1)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' 1 a = [a]
replicate' n a = [a] ++ replicate' (n-1) a

elem' :: Eq a => a -> [a] -> Bool
elem' el [] = False
elem' el [x] = el == x
elem' el (x:xs) = (el == x) || elem' el xs

-- 7
merge' :: Ord a => [a] -> [a] -> [a]
merge' [] [] = []
merge' [] ys = ys
merge' xs [] = xs
merge' (x:xs) (y:ys)
      | x < y = x:merge' xs (y:ys)
      | otherwise = y:merge' (x:xs) ys

-- 8
msort' :: Ord a => [a] -> [a]
msort' [] = []
msort' [a] = [a]
msort' xs = merge' (msort' xs1)  (msort' xs2) where
       (xs1, xs2) = halve xs
       halve xs = (take half xs, drop half xs)
       half = (length xs) `div` 2

-- 9
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' 0 xs = []
take' n [] = []
take' n (x:xs) = x : take' (n-1) xs

last' :: [a] -> a
last' [x] = x
last' (_:xs) = last' xs
