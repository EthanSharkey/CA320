
repFirst :: Eq a => a -> a -> [a] -> [a]

repFirst n m [] = []


repFirst n m (x : xs)
    | n == x = m : xs
    | otherwise = x : repFirst n m xs