-- 1
res1 = sum [x^2|x <- [1..100]]

-- 2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(m,n)|m <- [0..m], n <- [0..n]]

-- 3
square :: Int -> [(Int, Int)]
square n' = [(m,n)|(m,n) <- grid n' n', m /= n]

-- 4
replicate' :: Int -> a -> [a]
replicate' n x = [x|_ <- [0..n-1]]

-- 5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z)|x<-[1..n],y<-[1..n],z<-[1..n], x^2+y^2 == z^2]

-- 6
factors :: Int -> [Int]
factors n = [x|x<-[1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x|x<-[1..n], (sum (init (factors x))) == x]

-- 9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y|(x,y) <- zip xs ys]