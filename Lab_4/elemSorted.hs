elemSorted :: Ord a => a -> [a] -> Bool

elemSorted n [] = False

elemSorted n (x : xs)
    | n == x = True
    | n < x = False
    | otherwise = elemSorted n xs