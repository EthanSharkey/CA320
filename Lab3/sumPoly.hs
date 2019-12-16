sumPoly :: [Int] -> [Int] -> [Int]

sumPoly [] [] = []
sumPoly a [] = a
sumPoly [] b = b

sumPoly (a : as) (b : bs) = a + b : sumPoly as bs