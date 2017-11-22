-- 1
-- mapFilter f p xs = [f x| x <- xs, p x]
mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f p xs = map f (filter p xs)

-- 2
all' :: (a -> Bool) -> [a] -> Bool
all' f xs = foldl (\acc x -> acc && f x) True xs

any' :: (a -> Bool) -> [a] -> Bool
any' f xs = foldl (\acc x -> acc || f x) False xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs)
          | f x = x:takeWhile' f xs
          | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs)
          | f x = dropWhile' f xs
          | otherwise = xs

-- 3
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = foldr (\x acc -> case f x of
                      True -> x:acc
                      False -> acc) [] xs

-- 4
dec2int :: [Int] -> Int
dec2int ints = foldl (+) 0 [i*m | (i,m) <- zip ints (reverse(take (length ints) (iterate (*10) 1)))]

-- 5
curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \a -> (\b -> f (a,b))

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \(a,b) -> f a b

-- 6
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = unfold (\l -> length l == 0) (\l -> f (head l)) (\l -> tail l) xs

iterate'' :: (a -> a) -> a -> [a]
iterate'' f a = unfold (\x -> False) id f a

type Bit = Int
chop8'' :: [Bit] -> [[Bit]]
chop8'' bits = unfold (\b -> length b < 8) (take 8) (drop 8) bits

-- 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 [] = []
altMap f1 f2 xs = [if even i then f1 x else f2 x |(x,i) <- zip xs [0..]]

-- 10
luhnDouble' :: Int -> Int
luhnDouble' n | n * 2 > 9 = n * 2 - 9
              | otherwise = n * 2
luhn' :: [Int] -> Bool
luhn' ns = sum (altMap id luhnDouble' (reverse ns)) `mod` 10 == 0