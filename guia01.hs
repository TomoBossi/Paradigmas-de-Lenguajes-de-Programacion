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
(++>) xs ys = foldr (\z pRes -> [z] ++ pRes) ys xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x pRes -> if f x then x:pRes else pRes) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x pRes -> (f x):pRes) []

-- 3.2.
mejorSegún :: (a -> a -> Bool) -> [a] -> a
mejorSegún f = foldr1 (\x pRes -> select f x pRes) -- exception on empty tree
  where
    select :: (a -> a -> Bool) -> a -> a -> a
    select f x y
      | f x y = x
      | otherwise = y

-- 3.3.
sumasParciales :: Num a => [a] -> [a]
sumasParciales xs = foldl (\acc x -> if null acc then [x] else acc ++ [(last acc) + x]) [] xs

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
sumIndexes f = ((foldr (\(i, y) pRes -> if f i then y + pRes else pRes) 0) . enumerate)
-- sumIndexes f xs = foldr (\(i, y) pRes -> if f i then y + pRes else pRes) 0 (enumerate xs) -- Cuenta como usar foldr, no cuenta como recursión estructural

sumAlt :: (Eq a, Num a) => [a] -> a
sumAlt = foldr1 (-)

-- sumAlt' :: (Eq a, Num a) => [a] -> a
-- sumAlt' xs = (sumIndexes even xs) - (sumIndexes odd xs)

-- 3.5.
sumAltRev :: (Eq a, Num a) => [a] -> a
sumAltRev = foldl1 (flip (-))

-- 4.1.
permutaciones :: [a] -> [[a]] -- concatMap :: (a -> [b]) -> [a] -> [b] ... ([a] -> [[a]]) -> [[a]] -> [[a]]
permutaciones = foldr (\x pRes -> if null pRes then [[x]] else concatMap (insertEverywhere x) pRes) []
  where
    insertEverywhere :: a -> [a] -> [[a]]
    insertEverywhere x xs = foldr (\i pRes -> [(take i xs) ++ [x] ++ (drop i xs)] ++ pRes) [] [0..(length xs)]

-- 4.2.
partes :: [a] -> [[a]]
partes = foldr (\x pRes -> concatMap (concatInserted x) pRes) [[]]
  where
    concatInserted :: a -> [a] -> [[a]]
    concatInserted x xs = [xs] ++ [x:xs]

-- 4.3.
prefijos :: [a] -> [[a]]
prefijos = foldr (\x pRes -> [[]] ++ (map ([x]++) pRes)) [[]]

-- 4.4.
sublistas :: Eq a => [a] -> [[a]]
sublistas xs = foldr (\i pRes -> (slidingWindow i (last pRes)) ++ pRes) [[], xs] [1..(length xs)-1]

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n xs = foldr (\i pRes -> [drop (i-n) (take i xs)] ++ pRes) [] [n..(length xs)]

-- sublistas' :: Eq a => [a] -> [[a]]
-- sublistas' xs = foldr (\_ pRes -> filterRecr (\z zs -> elem z zs) (concatMap (\ys -> [tail ys, init ys, ys]) pRes)) [xs] xs

filterRecr :: (a -> [a] -> Bool) -> [a] -> [a]
filterRecr f = recr (\x xs pRes -> if f x xs then pRes else x:pRes) []

-- 5.
-- elementosEnPosicionesPares no es recursión estructural, por aplicar tail sobre xs

-- Aún así, con preprocesamiento del input puedo reescribir elementosEnPosicionesPares con foldr:

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares x = foldr (\(i, y) pRes -> if even i then y:pRes else pRes) [] (enumerate x)

-- Esto no implica que sea recursión estructural, porque tuve que hacer un preprocesamiento al input.

-- entrelazar parecería tampoco serlo, porque no devuelve una valor constante (depende del segundo parámetro)... pero...

entrelazar' :: [a] -> [a] -> [a]
entrelazar' = foldr (\x pRes -> (\ys -> if null ys then x:pRes [] else x:head ys:pRes (tail ys))) id

--- En realidad devuelve una constante, una función constante! Waw!

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

genLista :: a -> (a -> a) -> Integer -> [a]
genLista initial f n = foldl (\acc _ -> if null acc then [initial] else acc ++ [f (last acc)]) [] (replicate (fromIntegral n) initial)

-- genLista' :: a -> (a -> a) -> Integer -> [a] -- Recursion explicita
-- genLista' initial _ 0 = [initial]
-- genLista' initial f n = xs ++ [f (last xs)]
--   where 
--     xs = genLista' initial f (n-1)

-- 7.2.
desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta lower upper = genLista lower (1+) (upper-lower) 

-- 8.1.
mapPares :: ((a, b) -> c) -> [(a, b)] -> [c]
mapPares f = foldr (\x pRes -> (f x):pRes) []

-- 8.2.
armarPares :: [a] -> [b] -> [(a, b)]
armarPares = foldr (\x pRes -> (\ys -> if null ys then pRes [] else (x, head ys):(pRes (tail ys)))) (\ys -> [])

-- armarPares :: [a] -> [b] -> [(a, b)] -- Recursion explicita
-- armarPares [] _ = []
-- armarPares _ [] = []
-- armarPares (x:xs) (y:ys) = (x, y):(armarPares xs ys)

-- 8.3.
mapDoble :: ((a, b) -> c) -> [a] -> [b] -> [c]
mapDoble f x y = mapPares f (armarPares x y)

-- 9.1.
sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = (zipWith . zipWith) (+) -- :O !!!

-- sumaMat''' :: [[Int]] -> [[Int]] -> [[Int]]
-- sumaMat''' x y = mapPares (\(xi, yi) -> (zipWith (\xij yij -> xij + yij) xi yi)) (armarPares x y)

-- sumaMat'' :: [[Int]] -> [[Int]] -> [[Int]]
-- sumaMat'' x y = zipWith (\xi yi -> (zipWith (\xij yij -> xij + yij) xi yi)) x y

-- sumaMat' :: [[Int]] -> [[Int]] -> [[Int]]
-- sumaMat' = zipWith (\xi yi -> (zipWith (\xij yij -> xij + yij) xi yi))

-- 9.2.
trasponer :: [[a]] -> [[a]]
trasponer xs = foldr (\x pRes -> concatSublists (column x) pRes) (emptySublists xs) xs
  where
    emptySublists :: [[a]] -> [[a]]
    emptySublists xs = replicate (length (head xs)) []
    column :: [a] -> [[a]]
    column = foldr (\x pRes -> [[x]] ++ pRes) []
    concatSublists :: [[a]] -> [[a]] -> [[a]]
    concatSublists xs ys = mapPares (\(x, y) -> x ++ y) (armarPares xs ys)

-- trasponer' :: [[a]] -> [[a]] -- Recursión explicita
-- trasponer' m
--   | null (concatSublists m) = []
--   | otherwise = (firstElems):(trasponer' remainingElems)
--   where
--     concatSublists :: [[a]] -> [a]
--     concatSublists = foldr (\x pRes -> x ++ pRes) []
--     firstElems = map head m
--     remainingElems = map tail m

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
foldNat :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer
foldNat _ z 0 = z
foldNat f z n = f n (foldNat f z (n - 1))

-- 11.2.
potencia :: Integer -> Integer -> Integer
potencia n m = foldNat (\_ pRes -> n*pRes) 1 m -- n**m

-- 12.1.
data Polinomio a = X | Cte a | Suma (Polinomio a) (Polinomio a) | Prod (Polinomio a) (Polinomio a)

-- evaluar :: Num a => a -> Polinomio a -> a
-- evaluar x (X) = x
-- evaluar x (Cte c) = c 
-- evaluar x (Suma p1 p2) = (evaluar x p1) + (evaluar x p2)
-- evaluar x (Prod p1 p2) = (evaluar x p1) * (evaluar x p2)

foldPol :: (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> b -> Polinomio a -> b
foldPol _ _ _ x (X) = x
foldPol f _ _ _ (Cte c) = f c
foldPol f fSuma fProd x (Suma p1 p2) = fSuma (foldPol f fSuma fProd x p1) (foldPol f fSuma fProd x p2)
foldPol f fSuma fProd x (Prod p1 p2) = fProd (foldPol f fSuma fProd x p1) (foldPol f fSuma fProd x p2)

evaluar :: Num a => a -> Polinomio a -> a
evaluar = foldPol id (+) (*)

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

-- espejo'' :: AB a -> AB a
-- espejo'' Nil = Nil
-- espejo'' (Bin l n r) = Bin (espejo'' r) n (espejo'' l)

-- 14.2.

mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura = foldAB (\pRes1 _ pRes2 -> (\ab -> case ab of {Nil -> False; Bin l _ r -> pRes1 l && pRes2 r})) (\ab -> esNil ab) -- Hacer esto me hizo muy felíz :)

-- mismaEstructura' :: AB a -> AB b -> Bool -- Recursión explicita
-- mismaEstructura' Nil Nil = True
-- mismaEstructura' _ Nil = False
-- mismaEstructura' Nil _ = False
-- mismaEstructura' (Bin l1 _ r1) (Bin l2 _ r2) = (mismaEstructura' l1 l2) && (mismaEstructura' r1 r2)

-- 15.a.
data AIH a = Hoja a | BinAIH (AIH a) (AIH a) deriving Show

foldAIH :: (b -> b -> b) -> (a -> b) -> AIH a -> b
foldAIH f fLeaf (Hoja h) = fLeaf h
foldAIH f fLeaf (BinAIH l r) = f (foldAIH f fLeaf l) (foldAIH f fLeaf r)

-- 15.b.
alturaAIH :: AIH a -> Integer
alturaAIH = foldAIH (\pRes1 pRes2 -> max pRes1 pRes2 + 1) (\h -> 1)

tamañoAIH :: AIH a -> Integer
tamañoAIH = foldAIH (\pRes1 pRes2 -> pRes1 + pRes2) (\h -> 1)

-- 15.c.
listUnitAIH :: [AIH ()]
listUnitAIH = generateUnitAIH nextUnitAIH

generateUnitAIH :: ([AIH ()] -> AIH ()) -> [AIH ()]
generateUnitAIH next = generateUnitAIHfrom next [Hoja ()]

generateUnitAIHfrom :: ([AIH ()] -> AIH ()) -> [AIH ()] -> [AIH ()]
generateUnitAIHfrom next xs = (head xs):(generateUnitAIHfrom next ((next xs):xs))

nextUnitAIH :: [AIH ()] -> AIH ()
nextUnitAIH xs = nextUnitAIHhelper (head xs) (alturaAIH (head xs)) (tamañoAIH (head xs))
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
data RoseTree a = Leaf a | Tree [RoseTree a] deriving Show -- Duda: Diferente a lo hecho en la práctica, ¿vale igual?

-- 16.2.
foldRT :: (b -> b -> b) -> (a -> b) -> b -> RoseTree a -> b
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

-- teórica 22/03/2024
mapAB :: (a -> b) -> AB a -> AB b
mapAB f = foldAB (\pRes1 n pRes2 -> (Bin pRes1 (f n) pRes2)) Nil

main :: IO ()
main = do
  print ()