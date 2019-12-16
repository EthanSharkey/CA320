data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Enum,Show)

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
            deriving (Enum,Read)

type Date = (Int, Month, Int)



leap :: Int -> Bool

leap n
    | (n `mod` 400 == 0) && (n `mod` 100 == 0) = True
    | (n `mod` 4 == 0) && (n `mod` 100 == 0) = False
    | not (n `mod` 4 == 0) = False
    | otherwise = True


mLengths :: Int -> [Int]

mLengths n
    | leap n = [31,29,31,30,31,30,31,31,30,31,30,31]
    | otherwise = [31,28,31,30,31,30,31,31,30,31,30,31]
