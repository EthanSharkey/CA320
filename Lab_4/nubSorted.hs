import Data.List

nubSorted :: Eq a => [a] -> [a]

nubSorted [] = []
nubSorted [a] = [a]

nubSorted (x : xs)
    | x == head xs = nubSorted xs
    | otherwise = x : nubSorted xs
