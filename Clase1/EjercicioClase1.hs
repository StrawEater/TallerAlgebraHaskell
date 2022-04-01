{-
Taller De Algebra
Ejercicios Clase 1
-}


--1) absoluto: calcula el valor absoluto de un numero entero.

absInt :: Int -> Int
absInt x | x >= 0 = x
         | otherwise = (-x)

{-
--2) maximoabsoluto: devuelve el maximo
     entre el valor absoluto de dos numeros enteros.
-}

maximoabsoluto :: Int -> Int -> Int
maximoabsoluto x y | (absX) >= (absY) = (absX)
                   | otherwise = (absY)
                   where
                     absX = absInt x
                     absY = absInt y

{-
--3) maximo3: devuelve el maximo entre tres numeros enteros.
-}

maximo :: Int -> Int -> Int
maximo x y | x >= y = x
           | otherwise = y

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z = (maximo (maximo x y) z)

{-
--4) algunoEs0: dados dos numeros racionales,
     decide si alguno de los dos es igual a 0
     (hacerlo dos veces, una sin usar y otra usando pattern matching).
-}

algunoEs0Pattern :: Float -> Float -> Bool
algunoEs0Pattern _ 0 = True
algunoEs0Pattern 0 _ = True
algunoEs0Pattern x y = False

algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y = x * y == 0

{-
--5) ambosSon0: dados dos numeros racionales,
     decide si ambos son 0
     (hacerlo dos veces, una sin usar y otra usando pattern matching).
-}

ambosSon0Pattern :: Float -> Float -> Bool
ambosSon0Pattern 0 0 = True
ambosSon0Pattern x y = False

ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y = (abs x) + (abs y) == 0

{-
--6) esMultiploDe: dados dos numeros naturales,
     decidir si el primero es multiplo del segundo
-}

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = (mod x y) == 0

{-
--7) digitoUnidades: dado un numero natural, extrae su dıgito de las unidades.
-}

digitoUnidades :: Int -> Int
digitoUnidades x = (mod x 10)

{-
--8) digitoDecenas: dado un numero natural, extrae su dıgito de las decenas.
-}

digitoDecenas :: Int -> Int
digitoDecenas x = (div (mod x 100) 10)
