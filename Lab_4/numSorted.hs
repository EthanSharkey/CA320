numSorted :: Ord a => a -> [a] -> Int

numSorted n [] = 0



numSorted n (x : xs)
    | n == x && n <= (head xs) = 1 + numSorted n xs
    | otherwise = numSorted n xs