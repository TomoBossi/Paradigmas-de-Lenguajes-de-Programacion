import Test.HUnit

{-- Tipos --}

import Data.Either
import Data.List

data Dirección = Norte | Sur | Este | Oeste
  deriving (Eq, Show)

type Posición = (Float, Float)

data Personaje = Personaje Posición String  -- posición inicial, nombre
  | Mueve Personaje Dirección               -- personaje que se mueve, dirección en la que se mueve
  | Muere Personaje                         -- personaje que muere
  deriving (Eq, Show)

data Objeto = Objeto Posición String        -- posición inicial, nombre
  | Tomado Objeto Personaje                 -- objeto que es tomado, personaje que lo tomó
  | EsDestruido Objeto                      -- objeto que es destruido
  deriving (Eq, Show)

type Universo = [Either Personaje Objeto]

{-- Observadores y funciones básicas de los tipos --}

siguiente_posición :: Posición -> Dirección -> Posición
siguiente_posición p Norte = (fst p, snd p + 1)
siguiente_posición p Sur = (fst p, snd p - 1)
siguiente_posición p Este = (fst p + 1, snd p)
siguiente_posición p Oeste = (fst p - 1, snd p)

posición :: Either Personaje Objeto -> Posición
posición (Left p) = posición_personaje p
posición (Right o) = posición_objeto o

posición_objeto :: Objeto -> Posición
posición_objeto = foldObjeto const (const posición_personaje) id

nombre :: Either Personaje Objeto -> String
nombre (Left p) = nombre_personaje p
nombre (Right o) = nombre_objeto o

nombre_personaje :: Personaje -> String
nombre_personaje = foldPersonaje (const id) const id

está_vivo :: Personaje -> Bool
está_vivo = foldPersonaje (const (const True)) (const (const True)) (const False)

fue_destruido :: Objeto -> Bool
fue_destruido = foldObjeto (const (const False)) const (const True)

universo_con :: [Personaje] -> [Objeto] -> [Either Personaje Objeto]
universo_con ps os = map Left ps ++ map Right os

es_un_objeto :: Either Personaje Objeto -> Bool
es_un_objeto (Left p) = False
es_un_objeto (Right o) = True

es_un_personaje :: Either Personaje Objeto -> Bool
es_un_personaje (Left p) = True
es_un_personaje (Right o) = False

-- Asume que es un personaje
personaje_de :: Either Personaje Objeto -> Personaje
personaje_de (Left p) = p

-- Asume que es un objeto
objeto_de :: Either Personaje Objeto -> Objeto
objeto_de (Right o) = o

en_posesión_de :: String -> Objeto -> Bool
en_posesión_de n = foldObjeto (const (const False)) (\ r p -> nombre_personaje p == n) (const False)

objeto_libre :: Objeto -> Bool
objeto_libre = foldObjeto (const (const True)) (const (const False)) (const False)

norma2 :: (Float, Float) -> (Float, Float) -> Float
norma2 p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

cantidad_de_objetos :: Universo -> Int
cantidad_de_objetos = length . objetos_en

cantidad_de_personajes :: Universo -> Int
cantidad_de_personajes = length . personajes_en

distancia :: (Either Personaje Objeto) -> (Either Personaje Objeto) -> Float
distancia e1 e2 = norma2 (posición e1) (posición e2)

objetos_libres_en :: Universo -> [Objeto]
objetos_libres_en u = filter objeto_libre (objetos_en u)

está_el_personaje :: String -> Universo -> Bool
está_el_personaje n = foldr (\x r -> es_un_personaje x && nombre x == n && (está_vivo $ personaje_de x) || r) False

está_el_objeto :: String -> Universo -> Bool
está_el_objeto n = foldr (\x r -> es_un_objeto x && nombre x == n && not (fue_destruido $ objeto_de x) || r) False

-- Asume que el personaje está
personaje_de_nombre :: String -> Universo -> Personaje
personaje_de_nombre n u = foldr1 (\x1 x2 -> if nombre_personaje x1 == n then x1 else x2) (personajes_en u)

-- Asume que el objeto está
objeto_de_nombre :: String -> Universo -> Objeto
objeto_de_nombre n u = foldr1 (\x1 x2 -> if nombre_objeto x1 == n then x1 else x2) (objetos_en u)

es_una_gema :: Objeto -> Bool
es_una_gema o = isPrefixOf "Gema de" (nombre_objeto o)

{-Ejercicio 1-}

foldPersonaje :: (Posición -> String -> b) -> (b -> Dirección -> b) -> (b -> b) -> Personaje -> b
foldPersonaje f_ini _ _ (Personaje pos n) = f_ini pos n
foldPersonaje f_ini f_mov f_rip (Mueve p dir) = f_mov (foldPersonaje f_ini f_mov f_rip p) dir
foldPersonaje f_ini f_mov f_rip (Muere p) = f_rip (foldPersonaje f_ini f_mov f_rip p)

foldObjeto :: (Posición -> String -> b) -> (b -> Personaje -> b) -> (b -> b) -> Objeto -> b
foldObjeto f_ini _ _ (Objeto pos n) = f_ini pos n
foldObjeto f_ini f_toma f_des (Tomado o p) = f_toma (foldObjeto f_ini f_toma f_des o) p
foldObjeto f_ini f_toma f_des (EsDestruido o) = f_des (foldObjeto f_ini f_toma f_des o)

{-Ejercicio 2-}

posición_personaje :: Personaje -> Posición
posición_personaje = foldPersonaje const (\rec dir -> siguiente_posición rec dir) id

nombre_objeto :: Objeto -> String
nombre_objeto = foldObjeto (const id) const id

{-Ejercicio 3-}

objetos_en :: Universo -> [Objeto]
objetos_en u = map objeto_de (filter es_un_objeto u) -- foldr (\x rec -> either id (\o -> x:rec) x) []

personajes_en :: Universo -> [Personaje]
personajes_en u = map personaje_de (filter es_un_personaje u) -- foldr (\x rec -> either (\p -> x:rec) id []

{-Ejercicio 4-}

objetos_en_posesión_de :: String -> Universo -> [Objeto]
objetos_en_posesión_de n u = filter (en_posesión_de n) (objetos_en u)

{-Ejercicio 5-}

dist :: Objeto -> Personaje -> Float
dist o p = norma2 (posición_objeto o) (posición_personaje p)

-- Asume que hay al menos un objeto
-- Podemos usar foldr1 gracias al supuesto de tener al menos 1 objeto en el universo.
objeto_libre_mas_cercano :: Personaje -> Universo -> Objeto
objeto_libre_mas_cercano p u = foldr1 (\o rec -> if dist o p < dist rec p then o else rec) (objetos_en u)

{-Ejercicio 6-}
-- No es necesario chequear elem "Thanos" (map nombre_personaje (personajes_en u)), 
-- si "Thanos" no está en el universo objetos_en_posesión_de devuelve lista vacía.
tiene_thanos_todas_las_gemas :: Universo -> Bool
tiene_thanos_todas_las_gemas u = length (filter es_una_gema (objetos_en_posesión_de "Thanos" u)) == 6

{-Ejercicio 7-}

podemos_ganarle_a_thanos :: Universo -> Bool
podemos_ganarle_a_thanos u = thanos_no_gano && (thor && stormbreaker || wanda && visión_posee_gema_mente)
  where
    thanos_no_gano = not (tiene_thanos_todas_las_gemas u)
    thor = elem "Thor" (map nombre_personaje (personajes_en u))
    stormbreaker = elem "StormBreaker" (map nombre_objeto (objetos_en u))
    wanda = elem "Wanda" (map nombre_personaje (personajes_en u))
    visión_posee_gema_mente = elem "Gema de la Mente" (objetos_en_posesión_de "Thanos" u)

{-Demostración ej. 3-}

{-

QVQ ∀ u :: Universo . ∀ o :: Objeto . elem o (objetos_en u) ⇒ elem (Right o) u

Llamamos P(u): "elem o (objetos_en u) ⇒ elem (Right o) u"

Primero nombramos a las ecuaciones conocidas para referenciarlas en la demostración:

.... elem :: Eq a => a -> t a -> Bool
{E0} elem e [] = False
{E1} elem e (x:xs) = e == x || elem e xs

.... objetos_en :: Universo -> [Objeto]
{OU} objetos_en u = map objeto_de (filter es_un_objeto u)

.... objeto_de :: Either Personaje Objeto -> Objeto
{OD} objeto_de (Right o) = o

.... map :: (a -> b) -> [a] -> [b]
{M0} map f [] = []
{M1} map f (x : xs) = (f x):(map f xs)

.... filter :: (a -> Bool) -> [a] -> [a]
{F0} filter p [] = []
{F1} filter p (x : xs) = if p x then x:(filter p xs) else filter p xs

..... es_un_objeto :: Either Personaje Objeto -> Bool
{ - } es_un_objeto (Left p) = False
{EOR} es_un_objeto (Right o) = True

Vamos a probar la propiedad por inducción en la lista u.

- Caso base: 
    QVQ vale P([]): "elem o (objetos_en []) ⇒ elem (Right o) []"
    
    Partiendo desde el antecedente:
    = elem o (map objeto_de (filter es_un_objeto [])) {OU}
    = elem o (map objeto_de []) {F0}
    = elem o [] {M0}
    = False {E0}

    Partiendo desde el consecuente:
    = False {E0}

    Juntando las expresiones a las que se llegó a partir de antecedente y consecuente,
    QVQ vale: "False ⇒ False"
    Esto es verdadero por definición de (⇒).

- Caso inductivo:
    QVQ vale P(xs) ⇒ P(x:xs): "(elem o (objetos_en xs) ⇒ elem (Right o) xs) ⇒ (elem o (objetos_en x:xs) ⇒ elem (Right o) x:xs)"
    Por lo tanto, nuestra hipótesis inductiva es:
    {HI} elem o (objetos_en xs) ⇒ elem (Right o) xs
    Y nuestra tésis inductiva es:
    {TI} elem o (objetos_en x:xs) ⇒ elem (Right o) x:xs

    Partiendo desde el antecedente de {TI}:
    = elem o (map objeto_de (filter es_un_objeto x:xs)) {OU}
    = elem o (map objeto_de (if es_un_objeto x then x:(filter es_un_objeto xs) else filter es_un_objeto xs)) {F1}
    Resulta conveniente separar en casos, de acuerdo al valor de es_un_objeto x:

    - Caso A1. es_un_objeto x = True (por {EOR}, esto implica (Right x') == x = True (*)):
        = elem o (map objeto_de x:(filter es_un_objeto xs)) {es_un_objeto x = True (Caso A1.)}
        = elem o (objeto_de x):(map objeto_de (filter es_un_objeto xs)) {M1}
        = elem o (objeto_de x):(objetos_en xs) {OU}
        = elem o x':(objetos_en xs) {es_un_objeto x = True ⇒ objeto_de x == x' = True (el antecedente vale por Caso A1.)}
        = (o == x') || elem o (objetos_en xs) {E1}
        Nuevamente resulta conveniente separar en casos, esta vez de acuerdo al valor de (o == x'):
        
        - Caso A1.1. (o == x') = True:
            = True {definición de ||}

        - Caso A1.2. (o == x') = False:
            = elem o (objetos_en xs) {definición de ||}

    - Caso A2. es_un_objeto x = False:
        = elem o (map objeto_de (filter es_un_objeto xs)) {es_un_objeto x = False (Caso A2.)}
        = elem o (objetos_en xs) {OU}

    Partiendo desde el consecuente de {TI}:
    = ((Right o) == x) || elem (Right o) xs {E1}
    Separando en casos de acuerdo al valor de ((Right o) == x):

    - Caso C1. ((Right o) == x) = True:
        = True {definición de ||}

    - Caso C2. ((Right o) == x) = False:
        = elem (Right o) xs {definición de ||} 

    Juntando lo analizado partiendo desde el antecedente y en consecuente de {TI},
    necesitamos ver que sea verdadera toda combinación de casos no contradictoria:

        - Caso A1.1 && C1. es_un_objeto x = True && (o == x') = True && ((Right o) == x) = True:
            QVQ bajo este caso vale: "True ⇒ True"
            Esto es verdadero por definición de (⇒).

        - Caso A1.1 && C2. es_un_objeto x = True && (o == x') = True && ((Right o) == x) = False:
            Esta combinación de casos es contradictoria. Partiendo de (o == x') = True:
            = ((Right o) == (Right x')) = True
            = ((Right o) == x) = True
            Esto es contradictorio con la condición ((Right o) == x) = False,
            proveniente del caso C2.
            Esta combinación de casos por lo tanto es imposible,
            y en consecuencia queda descartada.

        - Caso A1.2. && C1. es_un_objeto x = True && (o == x') = False && ((Right o) == x) = True:
            Esta combinación de casos es contradictoria.
            La justificación es idéntica a la dada para la combinación de casos A1.1 && C2.

        - Caso A1.2. && C2. es_un_objeto x = True && (o == x') = False && ((Right o) == x) = False:
            QVQ bajo este caso vale: "elem o (objetos_en xs) ⇒ elem (Right o) xs"
            Esta expresión es exactamente nuestra {HI}, verdadera por supuesto.

    Dado que la propiedad vale en todas las combinaciones de casos no contradictorias,
    vale en el caso inductivo en general. Como además se demostró que vale en el caso base,
    queda demostrado por inducción que vale ∀ u :: Universo . ∀ o :: Objeto,
    como se quería probar.

    (*) Aclaración: x' :: Objeto es el resultado de objeto_de x cuando se sabe a priori que x' es un objeto
        Lo llamamos así para no confundirlo con x :: Either Personaje Objeto
        (Por la misma razón, también vale (Right x') == x)

-}

{-Tests-}

main :: IO Counts
main = do runTestTT allTests

allTests = test [ -- TODO Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7
  ]

phil = Personaje (0,0) "Phil"
mjölnir = Objeto (2,2) "Mjölnir"
universo_sin_thanos = universo_con [phil] [mjölnir]

testsEj1 = test [ -- Casos de test para el ejercicio 1
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) phil             -- Caso de test 1 - expresión a testear
    ~=? 0                                                               -- Caso de test 1 - resultado esperado
  ,
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) (Muere phil)     -- Caso de test 2 - expresión a testear
    ~=? 1                                                               -- Caso de test 2 - resultado esperado
  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  posición_personaje phil       -- Caso de test 1 - expresión a testear
    ~=? (0,0)                   -- Caso de test 1 - resultado esperado
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  objetos_en []       -- Caso de test 1 - expresión a testear
    ~=? []            -- Caso de test 1 - resultado esperado
  ]

testsEj4 = test [ -- Casos de test para el ejercicio 4
  objetos_en_posesión_de "Phil" []       -- Caso de test 1 - expresión a testear
    ~=? []                             -- Caso de test 1 - resultado esperado
  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5
  objeto_libre_mas_cercano phil [Right mjölnir]       -- Caso de test 1 - expresión a testear
    ~=? mjölnir                                       -- Caso de test 1 - resultado esperado
  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  tiene_thanos_todas_las_gemas universo_sin_thanos       -- Caso de test 1 - expresión a testear
    ~=? False                                            -- Caso de test 1 - resultado esperado
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
  podemos_ganarle_a_thanos universo_sin_thanos         -- Caso de test 1 - expresión a testear
    ~=? False                                          -- Caso de test 1 - resultado esperado
  ]