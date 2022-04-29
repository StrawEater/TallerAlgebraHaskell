--Listas
--Tipos Fijos
listaInt = [2,4,5]
listaBool = [True, False, True]
listaListaInt = [[13,45,3], [], [43,36,34,3]]
listaVacia = [] -- Lista vacia, no tiene tipo. Comodin

-- Si los numeros no tienen el mismo Tipo, salta Error
--listaError = [3, True]

-- head :: [a] -> a
-- Devuelve el index 0 de la lista dada

int3 = (head [3,4,5,6])
headInception = head (head listaListaInt) --13
-- head [] Error

-- tail :: [a] -> [a]
-- Devuelve una lista sin el primer elemento de la Lista original

listaTail = tail [1,2,3,4] -- [2, 3, 4]
listaVaciaAhora = tail [4] -- []
-- tail [] Error

-- : :: a -> [a] -> [a]
-- Dado un elemento y una lista del mismo tipo, agrega el elemento en el index 0

listaUnida = 0 : [1,2,3,4,5] -- [0,1,2,3,4,5]
listaDeLista = [1,2] : [[3,4]] -- [[1,2],[3,4]]
listaLlena = 2 : [] -- [2]

-- Dado un numero natural n, devuelve una lista de mayor a menor de n hasta 0
llenarLista :: Int -> [Int]
llenarLista n | n == 0 = [0]
              | otherwise = n : (llenarLista (n-1))

ordenImporta = head [1, 2, 3] : [4, 5] -- [1, 4, 5]
--ordenImporta2 = head ([1, 2, 3] : [4, 5]) -- Error, porque intenta meter [1, 2, 3] en [4, 5]

--Suma los elementos de una lista de Ints
sumatoria :: [Int] -> Int
sumatoria l | l == [] = 0
            | otherwise = (head l) + (sumatoria (tail l)) 

-- CON PATTERN MATCHING
-- Una lista se puede entender como x:xs, x siendo el primer elemento y xs el tail
-- [] si es lista vacia
-- Conviene para no hacer tail vos
sumatoriaPattern :: [Int] -> Int
sumatoriaPattern [] = 0
sumatoriaPattern (x:xs) = x + sumatoriaPattern(xs)


-- Dada una lista de Ints, devuelve la cantidad de elementos que contiene
longitud :: [Int] -> Int
longitud l | l == [] = 0
           | otherwise = 1 + (longitud (tail l)) 

-- CON PATTERN MATCHING
longitudPattern :: [Int] -> Int
longitudPattern [] = 0
longitudPattern (x:xs) = 1 + longitudPattern(xs)

-- o

longitudPattern2 :: [Int] -> Int
longitudPattern2 [] = 0
longitudPattern2 (_:xs) = 1 + longitudPattern2(xs)

-- Dado un numero n y una lista l, decide si n es un elemento de l
pertenece :: Int -> [Int] -> Bool
pertenece n l | l == [] = False
              | head l == n = True
              | otherwise = pertenece n (tail l)

-- CON PATTERN MATCHING 
pertenecePattern :: Int -> [Int] -> Bool
pertenecePattern n [] = False
pertenecePattern n (x:xs) = n == x || pertenecePattern n xs


lista1a100 = [1..100] --Devuelve una lista de numeros enteros que van de 1 a 100
listaCasiCompleta = [1,3..100] --Devuelve una lista [1,3] + todos los numeros entre 7 y 100
-- Solo funciona para el segundo elemento, despues ya no

listaDescendente = [100..1] --Devuelve una lista vacia, solo puede ser ascendente
listaMax = [1..] -- Devuelve una lista de 1 hasta el maxInt

-- Dada una lista de Ints, invierte el signo de sus elementos
getListaDescendente :: [Int] -> [Int] 
getListaDescendente l | l == [] = []
                      | otherwise = -(head l) : (getListaDescendente (tail l)) 

listaDescendente100 = getListaDescendente [-1..100]

primerMultiplode45345 :: [Int] -> Int
primerMultiplode45345 l | l == [] = 0
                        | mod (head l) 45345 == 0 = (head l)
                        | otherwise = primerMultiplode45345 (tail l)



