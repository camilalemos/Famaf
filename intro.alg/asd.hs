factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n -1)

segundo3 :: (a, b, c) -> b
segundo3 (x,y,z) = y

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y | mod y x==0 = True
                 | mod y x /=0 = False

esBisiesto :: Int -> Bool
esBisiesto x = ((mod x 400==0) || ((mod x 4 ==0) && (mod x 100/=0)))

max3 x y z = max (max x y) z
min3 x y z= min(min x y) z

dispersion :: Int -> Int -> Int -> Int
dispersion x y z = ( max3 x y z ) - (min3 x y z)

celsiusToFahr x= x*1.8 + 32
fahrToCelsius x= (x-32)/1.8

haceFrioF :: Float -> Bool
haceFrioF x = (fahrToCelsius x) < 8

soloPares :: [Int] -> [Int]
soloPares []=[]
soloPares (x: xs) | (mod x 2) ==0 = x: soloPares xs

mayoresQue :: [Int] -> Int -> [Int]
mayoresQue [] n = []
mayoresQue (x:xs) n | x>n = x: mayoresQue xs n
                    | x<= n = mayoresQue xs n

mayoresQue10 :: [Int] -> [Int]
mayoresQue10 x = mayoresQue x 10

sumar1 :: [Int] -> [Int]
sumar1 [] = []
sumar1 (x:xs) = (x+1) : sumar1 xs

duplica :: [Int] -> [Int]
duplica []= []
duplica (x:xs) = 2*x : duplica xs
 
multiplica :: [Int] -> Int -> [Int]
multiplica [] n = []
multiplica (x:xs) n = n*x : multiplica xs n 

hay0 :: [Int] -> Bool
hay0 [] = False
hay0 (x:xs) | x==0 = True
            | x/=0 = hay0 xs

todosMenores10 :: [Int] -> Bool
todosMenores10 [] = True
todosMenores10 (x:xs) | x<10 = todosMenores10 xs
                      | x>=10 = False

todosMenores10' :: [Int] -> Bool
todosMenores10' [] = True
todosMenores10' (x:xs) = x<10 && todosMenores10' xs


maximo:: [Int] -> Int
maximo [x]= x
maximo (x:xs)= max x (maximo xs)

repartir :: [String] -> [String] -> [(String,String)]
repartir [] [] = []
repartir [] (xs) = []
repartir (xs) [] = []
repartir (x:xs) (y:ys) = (x,y) : repartir xs ys

apellidos :: [(String,String,Int)] -> [String]
apellidos [] = []
apellidos ((a,b,c):xs) = b : apellidos xs

length2 :: [a] -> Int
length2 [] = 0
length2 (x:xs) = 1+ length2 xs

posicion :: [a] -> Int -> a
posicion (x:xs) 0 = x 
posicion (x:xs) n = posicion xs (n-1)

take1:: [a] -> Int -> [a]
take1 (x:xs) 0 = [x]
take1 (x:xs) n = x: take1 xs (n-1)

drop1:: [a] -> Int -> [a]
drop1 (x:xs) 0 = xs
drop1 (x:xs) n = drop1 xs (n-1)

concat2 :: [a] -> [a] -> [a]
concat2 [] ys = ys
concat2 (x:xs) ys = x : concat2 xs ys

pegarAtras:: [a] -> a -> [a]
pegarAtras [] x = [x]
pegarAtras (y:ys) x = y: pegarAtras ys x

repetir :: Int -> Int -> [Int]
repetir a 0 = [a]
repetir a n = a: repetir a (n-1)

ultimo:: [a] -> a
ultimo [] = undefined
ultimo (x:xs) | length xs==0 = x 
              | otherwise = ultimo xs

sumaPares:: [(Int,Int)] -> Int
sumaPares [] = 0
sumaPares ((a,b) :xs) = a+b+sumaPares xs

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (x:xs) = x ++ concat1 xs


 




                


