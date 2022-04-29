
--Devuelve el producto de todos los elementos, si [] devuelve 1
productoria :: [Int] -> Int
productoria [] = 0
productoria [a] = a
productoria (x:xs) = x * productoria (xs) 

-- Dado un Numero N y una lista, suma N a cada elemento de la lista
sumaN :: Int -> [Int] -> [Int]
sumaN n [] = []
sumaN n (x:xs) = (n+x) : sumaN n xs

-- Dada una lista no vacia, devuelve una lista con los elementos sumados por su primer elemento 
sumarElPrimero :: [Int] -> [Int]
sumarElPrimero [] = []
sumarElPrimero (x:xs) = sumaN x (x:xs)

--Dada una lista no vacia, devuelve el ultimo elemento de ella
conseguirUltimoElemento :: [Int] -> Int
conseguirUltimoElemento [x] = x
conseguirUltimoElemento (x:xs) = conseguirUltimoElemento xs

-- Dada una lista no vacia, devuelve una lista con los elementos sumados por su ultimo elemento 
sumarElUltimo :: [Int] -> [Int]
sumarElUltimo [] = []
sumarElUltimo l = sumaN (conseguirUltimoElemento l) l

-- Dada una lista no vacia, devuelve solo los elementos pares de la misma
pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) | mod x 2 == 0 = x : (pares xs)
             | otherwise = (pares xs)

-- Dada una lista no vacia, devuelve solo los elementos multiplos de n de la misma
multiploDeN :: Int -> [Int] -> [Int]
multiploDeN _ [] = []
multiploDeN n (x:xs) | mod x n == 0 = x : (multiploDeN n xs)
                     | otherwise = (multiploDeN n xs)

-- Dandole un numero n y una lista, devuelve una lista sin la primer instancia de n
quitar :: Int -> [Int] -> [Int]
quitar _ [] = []
quitar n (x:xs) | n /= x = (x : (quitar n xs))
                | otherwise = xs

-- Dandole un numero n y una lista, devuelve si el elemento se encuentra dentro de la lista
pertenece :: Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

-- True si la lista tiene elementos repetidos
-- Si yo saco la primer instacia de un elemento, y en la lista resultante sigue
-- habiendo una referencia a el, significa que aparece mas de una vez.
hayRepetidos :: [Int] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | (pertenece x (quitar x (x:xs))) == True = True
                    | otherwise = hayRepetidos xs

-- True si el numero aparece mas de una vez dentro de la lista
esRepetido :: Int -> [Int] -> Bool
esRepetido _ [] = False
esRepetido n l = pertenece n (quitar n l)

-- Dada un numero y una lista con elementos repetidos, devuelve una lista con elementos unicos
eliminarRepetidos :: [Int] -> [Int]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | esRepetido x (x:xs) == True = eliminarRepetidos (xs)
                         | otherwise = x : eliminarRepetidos (xs)

-- Devuelve el elementos mas grande de una lista
maximo :: [Int] -> Int
maximo [a] = a
maximo (x:xs) = max x (maximo xs)

-- Devuelve el elementos mas chico de una lista
minimo :: [Int] -> Int
minimo [a] = a
minimo (x:xs) = min x (minimo xs)

-- Ordena una lista de Ints en orden ascendente
ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar l = minNumber : ordenar (quitar minNumber l) 
                where 
                    minNumber = minimo l

-- Dado una lista, devuelve otra sin el ultimo elemento de la lista original
quitarUltimo :: [Int] -> [Int]
quitarUltimo [] = []
quitarUltimo [x] = []
quitarUltimo (x:xs) = x : quitarUltimo xs

-- Dada una lista, devuelve la lista con el orden inverso
reverso :: [Int] -> [Int]
reverso [] = []
reverso l = ultimoElemento : (reverso listaSinUltimo)
                where 
                    ultimoElemento = conseguirUltimoElemento l
                    listaSinUltimo = quitarUltimo l
