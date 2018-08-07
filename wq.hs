-- brute force
perms :: Int -> [[Int]]
perms n = [[s..f] | s <- [1..n], f <- [1..n], s < f]

conseqBrut :: Int -> Int
conseqBrut n = length [k | k <- perms n, sum k == n]

-- we dont actualy need this
sumsToX :: Int -> Int -> Int -> Bool
sumsToX from to x = sum [from..to] == x

-- optimized
sums :: Int -> Int -> Int -> Int
sums from to x
  | from >= x || to >= x = 0
  | sum [from..to] == x = 1 + sums (from+1) (from+2) x
  | sum [from..to] > x = sums (from+1) (from+2) x
  | otherwise = sums from (to+1) x

conseqOpt :: Int -> Int
conseqOpt n = sums 1 1 n


-- conseqOpt 1234
-- 1
-- (0.02 secs, 16,473,776 bytes)
-- conseqBrut 1234
-- 1
-- (12.64 secs, 44,041,657,712 bytes)
-- optimized is approx x632 faster, and uses x2,673 times less memory