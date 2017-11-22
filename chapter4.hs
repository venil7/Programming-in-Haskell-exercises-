halve :: [a] -> ([a],[a])
halve (xs) = (take h xs, drop h xs)
   where h = length xs `div` 2

third :: [a] -> a
third (a:b:c:xs) = c

safetail :: [a] -> [a]
safetail [] = []
safetail (x:xs) = xs

safetail1 xs | length xs == 0 = []
             | otherwise = tail xs


(||) :: Bool -> Bool -> Bool
True || _ = True
False || b = b

mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))


luhnDouble :: Int -> Int
luhnDouble i = if d > 9 then d - 9 else d
               where d = i * 2

luhn_ :: Int -> Int -> Int -> Int -> Bool
luhn_ a b c d = (sum [luhnDouble a, b, luhnDouble c, d] `mod` 10) == 0

luhn :: [Int] -> Bool
luhn xs = ((evens + odds) `mod` 10) == 0 where
      evens = sum [x | (x,i) <- reversed_indexed_xs, i `mod` 2 == 0]
      odds = sum [luhnDouble x | (x,i) <- reversed_indexed_xs, i `mod` 2 /= 0]
      reversed_indexed_xs = zip (reverse xs) [0..]
