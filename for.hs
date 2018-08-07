-- for 1 (<1x) (+1)
for :: a -> (a -> Bool) -> (a -> a) -> (a -> IO ()) -> IO ()
for i cond inc action
  | cond i  = do
    action i
    for (inc i) cond inc action
  | otherwise = return ()