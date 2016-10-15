x::Int
x=5

lucky :: (Integral a) => a -> String

lucky 7 =  "Lucky day"
lucky x = "Bad day"

factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x-1)

sumatoria :: Integer -> Integer
sumatoria 1 = 1
sumatoria x = x + sumatoria (x-1) 

sumET :: [Integer]->[Integer]
sumET []= []
sumET (x:[]) = [x]
sumET (x:y:zs) = (x+y):sumET zs

intLL::[Int]->Int
intLL [] = 0
intLL (x:zs) = 1 + intLL zs



wTest w
	|w<=40 = "estas flaco"
	|w<=60 = "Estas bien de peso"
    |w<=80 ="Estas gordo"
    |otherwise ="Estas super gordo :("

mayor :: Int ->Int ->Int
mayor a b = 
	if a > b 
		then a  
		else  b

dupPars xs = [x*2| x <- xs, mod x 2 == 0]

mulT xs = [ x |  x <- xs, mod x 3 == 0]