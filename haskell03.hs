-- Prática 03 de Haskell - Parte 3
-- Nome: Ivan Maidana da Silveira

--1
add10toall :: [Int] -> [Int]
add10toall list = [x+10 |  x <- list]

--2
multN :: Int -> [Int] -> [Int]
multN n list = [n * x | x <- list]

--3
mutipl :: Int -> Int -> Int
mutipl x y = x*y

multN' :: Int -> [Int] -> [Int]
multN' n list = map (mutipl n) list

--4
applyExpr :: [Int] -> [Int]
applyExpr list = [3*x+2 | x <- list]

--5
calc :: Int -> Int
calc x = 3*x+2

applyExpr' :: [Int] -> [Int]
applyExpr'= (\ list -> map calc list)

--6
addSuffix :: String -> [String] -> [String]
addSuffix str list = [x++str | x <- list]

--7
selectgt5 :: [Int] -> [Int]
selectgt5 list = [x | x <- list, x > 5]

--8
fl :: Int -> Int -> Int
fl x y = x+y

sumOdds :: [Int] -> Int
sumOdds list = foldl1 fl [x | x <- list, mod x 2 /= 0]

--9
eimpar :: Int -> Bool
eimpar x = if mod x 2 /= 0 then True else False

soma :: Int -> Int -> Int
soma x y = x+y

sumOdds' :: [Int] -> Int
sumOdds' list = foldl1 soma (filter eimpar list) 

--10
selectExpr :: [Int] -> [Int]
selectExpr list = [x| x <- list, x > 30, mod x 2 == 0, x < 50]

--11
countShorts :: [String] -> Int
countShorts list = length[x | x <- list, length x < 5]

--12
maior :: Float -> Bool
maior x = x > 10

calcExpr :: [Float] -> [Float]
calcExpr list = filter maior [x^2/2 | x <- list]

--13
esp :: Char -> Char
esp c = if c == ' ' then '-' else c 

trSpaces :: String -> String 
trSpaces str = map esp [ x | x <- str]

--14
segun :: (Int,Int) -> Int
segun y = snd y

selectSnd :: [(Int,Int)] -> [Int]
selectSnd x = map segun x

--15
mult :: Int-> Int -> Int
mult x y = x+y

dotProd :: [Int] -> [Int] -> Int
dotProd list lista =  foldl1 mult [ x * y | (x,y) <- (zip list lista)]
    