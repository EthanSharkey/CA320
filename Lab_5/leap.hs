data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday 
           deriving (Enum,Show)

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
            deriving (Enum,Read)

type Date = (Int, Month, Int)



countYear :: Int -> Int -> Int -> Int

countYear start end total
    | not (start == end) = countYear (start + 1) end (total + sum(mLengths (start - 1)))
    | otherwise = total

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


numDays :: Date -> Int

numDays (date, mon, year)
    | year == 1753 = date + sum(take(fromEnum mon) (mLengths year)) + (countYear 1753 year 0)
    | otherwise = date + sum(take(fromEnum mon) (mLengths year)) + (countYear 1753 year 0) - 1


dayOfWeek :: Date -> Day

dayOfWeek (date, mon, year)
    = toEnum ((numDays(date, mon, year) `mod` 7)) :: Day 