myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl _ acc [] = acc
myfoldl f acc (x:xs) = myfoldl f (f acc x) xs