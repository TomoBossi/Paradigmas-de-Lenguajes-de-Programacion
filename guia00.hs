import Data.Maybe
import Data.Either
data AB a = Nil | Bin (AB a) a (AB a) deriving Show

-- 1.
{-
null :: a -> Bool -- Estructura vacía
head :: [a] -> a -- Primer elemento
tail :: [a] -> [a] -- Todos menos el primero 
init :: [a] -> [a] -- Todos menos el último
last :: [a] -> a -- Último
take :: Int -> [a] -> [a] -- Elementos hasta el n-esimo
drop :: Int -> [a] -> [a] -- Elementos después del n-esimo
(++) :: [a] -> [a] -> [a] -- Concatenar 2 listas
concat :: [[a]] -> [a] -- Concatenar sublistas
(!!) :: [a] -> Int -> a -- Elemento en posición n-esima, contando desde 0
elem :: a -> [a] -> Bool -- Pertenece
-}

-- 2.a.
valorAbsoluto :: Float -> Float
valorAbsoluto x
  | x < 0 = -x
  | otherwise = x

-- 2.b.
bisiesto :: Int -> Bool
bisiesto a 
  | a `mod` 400 == 0 || (a `mod` 100 /= 0 && a `mod` 4 == 0) = True
  | otherwise = False

-- 2.c.
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

-- 2.d.
menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde 2 n
  where 
    menorDivisorDesde :: Int -> Int -> Int
    menorDivisorDesde m n 
      | m > n = n
      | mod n m == 0 = m
      | otherwise = menorDivisorDesde (m+1) n

esPrimo :: Int -> Bool
esPrimo n = n > 1 && n == menorDivisor n

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos x = cantDivisoresPrimosDesde x 2 0
  where
    cantDivisoresPrimosDesde :: Int -> Int -> Int -> Int
    cantDivisoresPrimosDesde x d res
      | x == 1 = res
      | x `mod` d == 0 && (x `div` d) `mod` d /= 0 = cantDivisoresPrimosDesde (x `div` d) (d+1) (res+1)
      | x `mod` d == 0 = cantDivisoresPrimosDesde (x `div` d) d res
      | otherwise = cantDivisoresPrimosDesde x (d+1) res

-- 3.a.
inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1.0/x)

-- 3.b.
beta :: Bool -> Int
beta True = 1
beta False = 0

aEntero :: Either Int Bool -> Int
aEntero x = either id beta x

-- 4.a.
limpiar :: String -> String -> String
limpiar caracteres [] = []
limpiar caracteres (x:xs)
  | x `elem` caracteres = limpiar caracteres xs
  | otherwise = [x] ++ limpiar caracteres xs

-- 4.b.
suma :: [Float] -> Float
suma [] = 0
suma (x:xs) = x + suma xs

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

promedio :: [Float] -> Float
promedio xs = (suma xs)/(fromIntegral (longitud xs))

difPromedio :: [Float] -> [Float]
difPromedio xs = difValor xs (promedio xs) 
  where
    difValor :: [Float] -> Float -> [Float]
    difValor [] _ = []
    difValor (x:xs) v = [x-v] ++ difValor xs v

-- 4.c.
todosIguales :: [Int] -> Bool
todosIguales (x:[]) = True
todosIguales (x:xs) = x == head xs && todosIguales xs

-- 5.a.
vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

-- 5.b.
showAB :: Show a => AB a -> String
showAB Nil = "Nil"
showAB (Bin (hijo_izq) nodo (hijo_der)) = "(" ++ (showAB hijo_izq) ++ ") " ++ (show nodo) ++ " (" ++ (showAB hijo_der) ++ ")"

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin (hijo_izq) nodo (hijo_der)) = Bin (negacionAB hijo_izq) (not nodo) (negacionAB hijo_der)

-- 5.c.
productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin (hijo_izq) nodo (hijo_der)) = (productoAB hijo_izq) * nodo * (productoAB hijo_der)

-- teórica 19/03/2024
merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _ [] [] = []
merge _ xs [] = xs
merge _ [] ys = ys
merge gt (x:xs) (y:ys)
  | x `gt` y = y : merge gt (x:xs) ys
  | otherwise = x : merge gt xs (y:ys)

mergesort :: (a -> a -> Bool) -> [a] -> [a]
mergesort gt (x1:x2:xs) = merge gt (mergesort gt (take l (x1:x2:xs))) (mergesort gt (drop l (x1:x2:xs)))
  where
    l = (longitud (x1:x2:xs)) `div` 2
mergesort _ xs = xs

main :: IO ()
main = do
  print ("2.a.", valorAbsoluto (-8.0), valorAbsoluto 5.0)
  print ("2.b.", bisiesto 1600, bisiesto 1700, bisiesto 1704)
  print ("2.c.", factorial 5)
  print ("2.d.", cantDivisoresPrimos (2*2*2*3*3*7))
  print ("3.a.", inverso 0.0, inverso 1.0, inverso 2.0)
  print ("3.b.", aEntero (Left 5), aEntero (Right True), aEntero (Right False))
  print ("4.a.", limpiar "susto" "puerta")
  print ("4.b.", difPromedio [2,3,4])
  print ("4.c.", todosIguales [2,3,4], todosIguales [3,3,3])
  print ("5.a.", vacioAB Nil, vacioAB (Bin (Bin (Nil) False (Bin (Nil) True (Nil))) True (Nil)))
  print ("5.b.", showAB (negacionAB (Nil::AB Bool)), showAB (negacionAB (Bin (Bin (Nil) False (Bin (Nil) True (Nil))) True (Nil))))
  print ("5.c.", productoAB Nil, productoAB (Bin (Bin (Nil) 3 (Bin (Nil) 5 (Nil))) 2 (Nil)))
  print("teo.merge", merge (>) [1,3,4,10,13,25] [1, 2,7,12,15,30])
  print("teo.mergesort", mergesort (>) [1,16,4,32,(-13),25,2])