dup :: Eq a => [a] -> Bool

dup [] = False
dup [a] = False

dup (x : xs)
    | x == last xs = True
    | x == head xs = Tru
    | otherwise = dup (init xs)

