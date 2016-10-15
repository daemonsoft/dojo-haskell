
toDigitsRev :: Int -> [Int]
toDigitsRev num = if num <= 0 then [] else (mod num 10) : toDigitsRev (div num 10)

toDigits :: Int -> [Int]
toDigits = map (read . (:[])) . show

intLL::[Int]->Int
intLL [] = 0
intLL (x:zs) = 1 + intLL zs


doubleEveryOther :: [Int]->[Int]
doubleEveryOther [] = []
doubleEveryOther n
    |(intLL n `mod` 2) == 0 = pair n
    |otherwise = nonpair n

pair :: [Int]->[Int]
pair []         = []
pair (x:xs:r)   = x * 2 : xs : doubleEveryOther r

nonpair :: [Int]->[Int]
nonpair (x:[])      = x:[]
nonpair (x:xs:r)    = x: xs * 2 : doubleEveryOther r

sumDigits :: [Int]->Int
sumDigits []    = 0
sumDigits (x:r) = sumNumber x + sumDigits r

sumNumber :: Int->Int
sumNumber n
    |n<10   =n
    |otherwise  = n `div` 10 + n `mod` 10

validate :: Int -> Bool 
validate n = mod (sumDigits(doubleEveryOther(toDigits (n)))) 10 == 0