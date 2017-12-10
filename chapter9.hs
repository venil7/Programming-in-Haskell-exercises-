subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices xs = [ x | xs' <- subs xs, x <- perms xs']

-- is_choice :: Eq a => [a] -> [a] -> Bool
-- is_choice xs ys =

  -- deletes first occurence
delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (y:ys)
          | x == y    = ys
          | otherwise = y:(delete x ys)