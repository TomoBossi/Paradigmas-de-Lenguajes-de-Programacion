-- 1.1.
{-
max :: Ord a => (a, a) -> a
normaVectorial :: (Float, Float) -> Float
subtract :: Float -> Float -> Float
predecesor :: Float -> Float
evaluarEnCero :: (Float -> a) -> a
dosVeces :: (Float -> a) -> a
flipAll :: [a -> b -> c] -> [b -> a -> c]
flipRaro :: b -> (a -> b -> c) -> a -> c
-}

-- 1.2.
{-
Sólo max y normaVectorial no están currificadas.
-}
max2 :: Ord a => a -> a -> a
max2 x y | x >= y = x
         | otherwise = y

normaVectorial :: Float -> Float -> Float
normaVectorial x y = sqrt (x^2 + y^2)

-- 2.1.
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

-- 2.2.
uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

-- 2.3.
-- Se puede para un N arbitrario concreto (ej. 2, 3, 5, 10, etc.), no para todo N por tipado.
-- Ej. 
curry5 :: ((a, b, c, d, e) -> f) -> a -> b -> c -> d -> e -> f
curry5 g v w x y z = g (v, w, x, y, z)

-- 3.1.
sum' :: Num a => [a] -> a
sum' = foldr (+) 0

elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr (\y partialResult -> if y == x then True else partialResult) False

addLast :: a -> [a] -> [a]
addLast y [] = [y]
addLast y (x:xs) = x:addLast y xs

(++>) :: [a] -> [a] -> [a]
(++>) x y = foldr (\z partialResult -> addLast z partialResult) x (reverse y)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x partialResult -> if f x then x:partialResult else partialResult) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x partialResult -> (f x):partialResult) []

-- 3.2.
mejorSegún' :: (a -> a -> Bool) -> [a] -> a
mejorSegún' f x = head (mejorParcialSegún f x)
  where
    mejorParcialSegún :: (a -> a -> Bool) -> [a] -> [a]
    mejorParcialSegún f = foldr (\x partialResult -> collapse f x partialResult) []
      where 
        collapse :: (a -> a -> Bool) -> a -> [a] -> [a]
        collapse f x [] = [x]
        collapse f x (y:ys) = if f x y then (x:ys) else (y:ys)

mejorSegún :: (a -> a -> Bool) -> [a] -> a
mejorSegún f y = foldr (\x partialResult -> if f x partialResult then x else partialResult) (last y) y -- exception on empty list

-- 3.3.
sumasParciales' :: Num a => [a] -> [a]
sumasParciales' x = foldr (\y partialResult -> if (null partialResult) then [y] else addLast (y + last partialResult) partialResult) [] (reverse x)

sumasParciales :: Num a => [a] -> [a]
sumasParciales (x1:x2:xs) = x1:(sumasParciales (x1+x2:xs))
sumasParciales (x:xs) = [x]
sumasParciales [] = []

-- 3.4.
enumerate :: Int -> [a] -> [(Int, a)]
enumerate i [] = []
enumerate i (x:xs) = (i, x):(enumerate (i+1) xs) 

sumIndexes :: (Eq a, Num a) => (Int -> Bool) -> [a] -> a
sumIndexes f x = foldr (\(i, y) partialResult -> if f i then y + partialResult else partialResult) 0 (enumerate 0 x) -- Duda: ¿Cuenta como usar foldr?

sumAlt :: (Eq a, Num a) => [a] -> a
sumAlt x = (sumIndexes even x) - (sumIndexes odd x)

-- 3.5.
sumAltRev :: (Eq a, Num a) => [a] -> a
sumAltRev x = sumAlt (reverse x) -- Probablemente no es lo que quiere la guía

-- 4.1.
-- 4.2.
-- 4.3.
-- 4.4.

-- 5.
-- elementosEnPosicionesPares no es recursión estructural, por aplicar tail sobre xs
-- Estoy tentado a decir que entrelazar tampoco lo es, porque no devuelve una valor constante (depende del segundo parámetro)...
-- Aún así, con preprocesamiento del input puedo reescribir elementosEnPosicionesPares con foldr:

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares x = foldr (\(i, y) partialResult -> if even i then y:partialResult else partialResult) [] (enumerate 0 x)

-- Duda: ¿Esto implica que es recursión estructural?

-- 6.a.
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs partialResult -> if x == e then xs else x:partialResult) []

-- 6.b.
-- Porque se requiere poder operar sobre xs (para devolverla) por fuera del llamado recursivo a sacarUna.

-- 6.c.
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs partialResult -> if e < x then e:x:xs else x:partialResult) []

-- 7.1.
-- 7.2.

-- 8.1.
mapPares :: ((a, b) -> c) -> [(a, b)] -> [c]
mapPares f = foldr (\x partialResult -> (f x):partialResult) []

-- 8.2.
armarPares :: [a] -> [b] -> [(a, b)]
armarPares [] _ = []
armarPares _ [] = []
armarPares (x:xs) (y:ys) = (x, y):(armarPares xs ys) 

-- 8.3.
mapDoble :: ((a, b) -> c) -> [a] -> [b] -> [c]
mapDoble f x y = mapPares f (armarPares x y)

-- 9.1.
sumaMat''' :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat''' x y = mapPares (\(xi, yi) -> (zipWith (\xij yij -> xij + yij) xi yi)) (armarPares x y)

sumaMat'' :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat'' x y = zipWith (\xi yi -> (zipWith (\xij yij -> xij + yij) xi yi)) x y

sumaMat' :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat' = zipWith (\xi yi -> (zipWith (\xij yij -> xij + yij) xi yi))

sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = (zipWith . zipWith) (+) -- :O

-- 9.2.
concatSublists :: [[a]] -> [a]
concatSublists = foldr (\x partialResult -> x ++ partialResult) []

trasponer :: [[a]] -> [[a]]
trasponer m
  | null (concatSublists m) = []
  | otherwise = (firstElems):(trasponer remainingElems)
  where
    firstElems = map head m
    remainingElems = map tail m

-- 10.1.
generate :: ([a] -> Bool) -> ([a] -> a) -> [a]
generate stop next = generateFrom stop next []

generateFrom :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom stop next xs 
  | stop xs = init xs
  | otherwise = generateFrom stop next (xs ++ [next xs])

generateBase :: ([a] -> Bool) -> a -> (a -> a) -> [a]
generateBase stop initial next = generate stop (\xs -> if null xs then initial else next (last xs))

-- 10.2.
factoriales :: Int -> [Int]
factoriales n = generate (\xs -> length xs - 1 == n) (\xs -> if null xs then 1 else (last xs)*(1 + length xs)) 

-- 10.3.
iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f x = generateBase (\xs -> length xs - 1 == n) x f

-- 10.4.
generateFrom' :: (a -> Bool) -> (a -> a) -> [a] -> [a] -- Tuve que cambiar los tipos para poder usar takeWhile e iterate
generateFrom' stop next xs = takeWhile (\ys -> (not . stop) ys) (iterate next (last xs))

-- 11.1.
foldNat :: (Integer -> Integer) -> Integer -> Integer -> Integer
foldNat _ z 0 = z
foldNat f z n = f (foldNat f z (n - 1))

-- 11.2.
potencia :: Integer -> Integer -> Integer
potencia n m = foldNat (n*) 1 m -- n**m

-- 12.1.
data Polinomio a = X | Cte a | Suma (Polinomio a) (Polinomio a) | Prod (Polinomio a) (Polinomio a)

evaluar :: Num a => a -> Polinomio a -> a
evaluar x (X) = x
evaluar x (Cte c) = c 
evaluar x (Suma p1 p2) = (evaluar x p1) + (evaluar x p2)
evaluar x (Prod p1 p2) = (evaluar x p1) * (evaluar x p2)

-- 13.1.
data AB a = Nil | Bin (AB a) a (AB a)

foldAB :: (b -> a -> b -> b) -> b -> AB a -> b -- (foldAB Bin Nil) es la identidad
foldAB _ zNil Nil = zNil
foldAB f zNil (Bin l n r) = f (foldAB f zNil l) n (foldAB f zNil r)

recAB :: (b -> AB a -> a -> AB a -> b -> b) -> b -> AB a -> b
recAB _ zNil Nil = zNil
recAB f zNil (Bin l n r) = f (recAB f zNil l) l n r (recAB f zNil r)

-- 13.2.
esNil :: AB a -> Bool
esNil Nil = True
esNil _ = False

altura :: AB a -> Int
altura = foldAB (\pRes1 x pRes2 -> max pRes1 pRes2 + 1) 0

cantNodos :: AB a -> Int
cantNodos = foldAB (\pRes1 x pRes2 -> pRes1 + 1 + pRes2) 0

-- 13.3.
mejorSegúnAB :: (a -> a -> Bool) -> AB a -> a
mejorSegúnAB f (Bin l n r) = foldAB (\pRes1 x pRes2 -> select f pRes1 x pRes2) n (Bin l n r) -- exception on empty tree
  where
    select :: (a -> a -> Bool) -> a -> a -> a -> a -- f l n r result
    select f x y z
      | f x y && f x z = x
      | f y x && f y z = y
      | f z x && f z y = z
      | otherwise = x

-- 13.4.
esABB :: Ord a => AB a -> Bool
esABB = recAB (\pRes1 l n r pRes2 -> pRes1 && pRes2 && ordChild l (<=) n && ordChild r (>=) n) True
  where
    ordChild :: Ord a => AB a -> (a -> a -> Bool) -> a -> Bool
    ordChild Nil _ _ = True
    ordChild (Bin _ nSub _) f n = f nSub n

-- 13.5.
-- esNil no requiere recursión.
-- altura, cantNodos y mejorSegúnAB no requieren operar sobre los subárboles directamente -> foldAB
-- esABB requiere operar sobre los subárboles directamente, en relación a su nodo -> recAB

-- 14.1.
-- 14.2.

-- 15.a.
data AIH a = Hoja a | BinI (AIH a) (AIH a)

-- 15.b.
-- 15.c.
-- 15.d.

-- 16.1.
-- 16.2.
-- 16.3.a.
-- 16.3.b.
-- 16.3.c.

main :: IO ()
main = do
  print ()