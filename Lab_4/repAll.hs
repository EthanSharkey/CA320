
repAll :: Eq a => a -> a -> [a] -> [a]

repAll n m [] = []


repAll n m (x : xs)
    | n == x = repAll n m (m : xs)
    | otherwise = x : repAll n m xs