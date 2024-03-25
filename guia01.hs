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
elem' x = foldr (\y pRes -> if y == x then True else pRes) False

addLast :: a -> [a] -> [a]
addLast y xs = xs ++ [y]

-- addLast' :: a -> [a] -> [a] -- Recursion explicita
-- addLast' y [] = [y]
-- addLast' y (x:xs) = x:addLast y xs

(++>) :: [a] -> [a] -> [a]
(++>) x y = foldr (\z pRes -> addLast z pRes) x (reverse y)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x pRes -> if f x then x:pRes else pRes) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x pRes -> (f x):pRes) []

-- 3.2.
mejorSegún :: (a -> a -> Bool) -> [a] -> a
mejorSegún f y = foldr (\x pRes -> select f x pRes) (last y) y -- exception on empty tree
  where
    select :: (a -> a -> Bool) -> a -> a -> a
    select f x y
      | f x y = x
      | otherwise = y

-- 3.3.
sumasParciales :: Num a => [a] -> [a]
sumasParciales xs = foldr (\y pRes -> if (null pRes) then [y] else addLast (y + last pRes) pRes) [] (reverse xs)

-- sumasParciales' :: Num a => [a] -> [a] -- Recursion explicita
-- sumasParciales' (x1:x2:xs) = x1:(sumasParciales' (x1+x2:xs))
-- sumasParciales' (x:xs) = [x]
-- sumasParciales' [] = []

-- 3.4.
enumerate :: [a] -> [(Int, a)]
enumerate xs = foldr (\x pRes -> if null pRes then [((length xs) - 1, x)] else (fst (head pRes) - 1, x):pRes) [] xs

-- enumerate' :: Int -> [a] -> [(Int, a)] -- Recursion explicita
-- enumerate' _ [] = []
-- enumerate' i (x:xs) = (i, x):(enumerate (i+1) xs) 

sumIndexes :: (Eq a, Num a) => (Int -> Bool) -> [a] -> a
sumIndexes f xs = foldr (\(i, y) pRes -> if f i then y + pRes else pRes) 0 (enumerate xs) -- Duda: ¿Cuenta como usar foldr?

sumAlt :: (Eq a, Num a) => [a] -> a
sumAlt xs = (sumIndexes even xs) - (sumIndexes odd xs)

-- 3.5.
sumAltRev :: (Eq a, Num a) => [a] -> a
sumAltRev x = sumAlt (reverse x) -- Probablemente no es lo que quiere la guía

-- 4.1.
permutaciones :: [a] -> [[a]] -- concatMap :: (a -> [b]) -> [a] -> [b]

-- 4.2.
partes :: [a] -> [[a]]

-- 4.3.
prefijos :: [a] -> [[a]]

-- 4.4.
sublistas :: [a] -> [[a]]

-- 5.
-- elementosEnPosicionesPares no es recursión estructural, por aplicar tail sobre xs
-- Estoy tentado a decir que entrelazar tampoco lo es, porque no devuelve una valor constante (depende del segundo parámetro)...
-- Aún así, con preprocesamiento del input puedo reescribir elementosEnPosicionesPares con foldr:

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares x = foldr (\(i, y) pRes -> if even i then y:pRes else pRes) [] (enumerate x)

-- Duda: ¿Esto implica que es recursión estructural?

-- 6.a.
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs pRes -> if x == e then xs else x:pRes) []

-- 6.b.
-- Porque se requiere poder operar sobre xs (para devolverla) por fuera del llamado recursivo a sacarUna.

-- 6.c.
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs pRes -> if e < x then e:x:xs else x:pRes) []

-- 7.1.
genLista :: a -> (a -> a) -> Integer -> [a] -- Recursion explicita
genLista initial _ 0 = [initial]
genLista initial f n = xs ++ [f (last xs)]
  where 
    xs = genLista initial f (n-1)

-- 7.2.
desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta lower upper = genLista lower (1+) (upper-lower) 

-- 8.1.
mapPares :: ((a, b) -> c) -> [(a, b)] -> [c]
mapPares f = foldr (\x pRes -> (f x):pRes) []

-- 8.2.
armarPares :: [a] -> [b] -> [(a, b)] -- Recursion explicita
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
concatSublists = foldr (\x pRes -> x ++ pRes) []

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

-- evaluar :: Num a => a -> Polinomio a -> a -- Duda: Recursion explicita?
-- evaluar x (X) = x
-- evaluar x (Cte c) = c 
-- evaluar x (Suma p1 p2) = (evaluar x p1) + (evaluar x p2)
-- evaluar x (Prod p1 p2) = (evaluar x p1) * (evaluar x p2)

foldPol :: (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> b -> Polinomio a -> b -- Duda: Recursion estructural?
foldPol _ _ _ x (X) = x
foldPol f _ _ _ (Cte c) = f c
foldPol f fSuma fProd x (Suma p1 p2) = fSuma (foldPol f fSuma fProd x p1) (foldPol f fSuma fProd x p2)
foldPol f fSuma fProd x (Prod p1 p2) = fProd (foldPol f fSuma fProd x p1) (foldPol f fSuma fProd x p2)

evaluar :: Num a => a -> Polinomio a -> a
evaluar x = foldPol (1*) (+) (*) x

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
altura = foldAB (\pRes1 n pRes2 -> max pRes1 pRes2 + 1) 0

cantNodos :: AB a -> Int
cantNodos = foldAB (\pRes1 n pRes2 -> pRes1 + 1 + pRes2) 0

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
ramas :: AB a -> [[a]]
-- ramas = foldAB (\pRes1 n pRes2 -> map (n:) (pRes1 ++ pRes2)) [[]]
ramas = recAB (\pRes1 l n r pRes2 -> if esHoja (Bin l n r) then [[n]] else map (n:) (pRes1 ++ pRes2)) []

esHoja :: AB a -> Bool
esHoja (Bin Nil _ Nil) = True
esHoja _ = False

cantHojas :: AB a -> Int
cantHojas = recAB (\pRes1 l n r pRes2 -> pRes1 + pRes2 + if (esNil l && esNil r) then 1 else 0) 0

espejo :: AB a -> AB a
espejo = foldAB (\pRes1 x pRes2 -> (Bin pRes2 x pRes1)) Nil

-- espejo' :: AB a -> AB a
-- espejo' Nil = Nil
-- espejo' (Bin l n r) = Bin (foldAB swap Nil r) n (foldAB swap Nil l)
--   where
--     swap = (\pRes1 x pRes2 -> (Bin pRes2 x pRes1))

-- espejo'' :: AB a -> AB a -- Duda: Recursion explicita?
-- espejo'' Nil = Nil
-- espejo'' (Bin l n r) = Bin (espejo'' r) n (espejo'' l)

-- 14.2.
mismaEstructura :: AB a -> AB b -> Bool -- Duda: Recursion explicita?
mismaEstructura Nil Nil = True
mismaEstructura _ Nil = False
mismaEstructura Nil _ = False
mismaEstructura (Bin l1 _ r1) (Bin l2 _ r2) = (mismaEstructura l1 l2) && (mismaEstructura r1 r2) 

-- 15.a.
data AIH a = Hoja a | BinAIH (AIH a) (AIH a) deriving Show

foldAIH :: (b -> b -> b) -> (a -> b) -> AIH a -> b
foldAIH f fLeaf (Hoja h) = fLeaf h -- No es estructural, pero no se me ocurre otra forma
foldAIH f fLeaf (BinAIH l r) = f (foldAIH f fLeaf l) (foldAIH f fLeaf r)

-- foldAIH' :: (b -> b -> b) -> b -> AIH a -> b
-- foldAIH' _ z (Hoja _) = z -- Es estructural, pero se pierde toda información de las hojas (sirve para el inciso b, pero me parece 'nacagada)
-- foldAIH' f z (BinAIH l r) = f (foldAIH' f z l) (foldAIH' f z r)

-- 15.b.
alturaAIH :: AIH a -> Integer
alturaAIH = foldAIH (\pRes1 pRes2 -> max pRes1 pRes2 + 1) (\h -> 1) -- Con foldAIH', el último valor sería directamente 1

tamañoAIH :: AIH a -> Integer
tamañoAIH = foldAIH (\pRes1 pRes2 -> pRes1 + pRes2) (\h -> 1) -- Con foldAIH', el último valor sería directamente 1

-- 15.c.
listUnitAIH :: Int -> [AIH ()] -- No lo hice infinito (sin el Int) porque el programa se colgaba a pesar de querer hacer take x listUnitAIH
listUnitAIH n = generateUnitAIH (\xs -> length xs > n) nextUnitAIH

generateUnitAIH :: ([AIH ()] -> Bool) -> ([AIH ()] -> AIH ()) -> [AIH ()]
generateUnitAIH stop next = generateUnitAIHfrom stop next [Hoja ()]

generateUnitAIHfrom :: ([AIH ()] -> Bool) -> ([AIH ()] -> AIH ()) -> [AIH ()] -> [AIH ()]
generateUnitAIHfrom stop next xs
  | stop xs = init xs
  | otherwise = generateUnitAIHfrom stop next (xs ++ [next xs])

nextUnitAIH :: [AIH ()] -> AIH ()
nextUnitAIH xs = nextUnitAIHhelper (last xs) (alturaAIH (last xs)) (tamañoAIH (last xs))
  where 
    nextUnitAIHhelper:: AIH () -> Integer -> Integer -> AIH ()
    nextUnitAIHhelper x h s
      | 2^(h-1) > s = addNextUnitAIH x (h-1)
      | otherwise = addNextLevelUnitAIH x

addNextUnitAIH :: AIH () -> Integer -> AIH ()
addNextUnitAIH (BinAIH (Hoja ()) (Hoja ())) 1 = BinAIH (Hoja ()) (Hoja ())
addNextUnitAIH (BinAIH (Hoja ()) (Hoja ())) _ = BinAIH (BinAIH (Hoja ()) (Hoja ())) (Hoja ())
addNextUnitAIH (BinAIH l (Hoja ())) _ = BinAIH l (BinAIH (Hoja ()) (Hoja ()))
addNextUnitAIH (BinAIH l r) c = BinAIH (addNextUnitAIH l (c-1)) (addNextUnitAIH r (c-1))
addNextUnitAIH (Hoja ()) _ = Hoja ()

addNextLevelUnitAIH :: AIH () -> AIH ()
addNextLevelUnitAIH (Hoja ()) = BinAIH (Hoja ()) (Hoja ())
addNextLevelUnitAIH (BinAIH l r) = BinAIH (addNextLevelUnitAIH l) r

-- 15.d.
-- No hay caso base en el que se retorne una constante

-- 16.1.
data RoseTree a = Leaf a | Tree [RoseTree a] deriving Show

-- 16.2.
foldRT :: (b -> b -> b) -> (a -> b) -> b -> RoseTree a -> b -- Mismo problema que en foldAIH... ¿Creo? nuevamente, 16.3.a,b,c resolubles devolviendo constante en las hojas
foldRT _ _ z (Tree []) = z
foldRT _ fLeaf _ (Leaf l) = fLeaf l
foldRT f fLeaf z (Tree (x:xs)) = f (foldRT f fLeaf z x) (foldRT f fLeaf z (Tree xs))

-- 16.3.a.
hojasRT :: RoseTree a -> [a]
hojasRT = foldRT (\x pRes -> x ++ pRes) (\x -> [x]) []

-- 16.3.b.
distanciasRT :: RoseTree a -> [Int]
distanciasRT = foldRT (\x pRes -> (map (1+) x) ++ pRes) (\x -> [0]) []

-- 16.3.c.
alturaRT :: RoseTree a -> Int
alturaRT t = 1 + (foldRT (\x pRes -> max (x+1) pRes) (\x -> 0) 0 t)

-- alturaRT' :: RoseTree a -> Int
-- alturaRT' t = mejorSegún (>) (distanciasRT t) + 1

-- práctica 22/03/2024
mapAB :: (a -> b) -> AB a -> AB b
mapAB f = foldAB (\pRes1 n pRes2 -> (Bin pRes1 (f n) pRes2)) Nil

main :: IO ()
main = do
  print (desdeHasta 5 10)