
delFirst :: Eq a => a -> [a] -> [a]

delFirst n [] = []


delFirst n (x : xs)
    | n == x = xs
    | otherwise =  x : delFirst n xs



    