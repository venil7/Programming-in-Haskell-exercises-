import System.IO

-- 1
putStr' :: String -> IO ()
putStr' str = sequence_ [putChar c | c <- str]

-- 2
type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard = putBoard' 1

putBoard' :: Int -> Board -> IO ()
putBoard' n [] = return ()
putBoard' n (x:xs) = do putRow n x
                        putBoard' (n+1) xs

-- 3
putBoard3 :: Board -> IO ()
putBoard3 xs = sequence_ [putRow n x | (x,n) <- zip xs [1..]]

-- 4
adder :: Int -> IO ()
adder n = adder' n 0

adder' :: Int -> Int -> IO ()
adder' remains total = do
  putStr "Enter number: "
  line <- getLine
  let total' = total + ((read line) :: Int)
  if remains > 1 then
    do adder' (remains-1) total'
  else
    do
      putStr "total: "
      putStrLn (show total')

-- 5
adder5 :: Int -> IO ()
adder5 n = do
  nums <- sequence [adder5' | _ <- [1..n]]
  putStr "total: "
  putStrLn (show $ sum nums)

adder5' :: IO Int
adder5' = do
  putStr "Enter number: "
  line <- getLine
  return ((read line) :: Int)

-- 6
getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x


readLine' :: IO String
readLine' = readLine'' ""

safe_init :: [a] -> [a]
safe_init [] = []
safe_init xs = init xs

readLine'' :: String -> IO String
readLine'' s = do
  ch <- getCh
  case ch of
    '\n' -> do
      putStrLn ""
      return s
    '\DEL' -> do
      let s' = safe_init s
      putStr ['\b', ' ', '\b']
      readLine'' s'
    _ -> do
      let s' = s ++ [ch]
      putStr [ch]
      readLine'' s'