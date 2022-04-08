
{-
Ejercicio 1:
Implementar la funcion fib : Z≥0 → Z que devuelve el i- ́esimo numero de Fibonacci.
-}

fib :: Int -> Int
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib(n-1) + fib(n-2)

{-
Ejercicio 2:
Implementar una funcion parteEntera :: Float -> Integer que calcule la parte entera
de un numero real positivo.
-}

parteEntera :: Float -> Int
parteEntera n | n >= 1 = 1 + (parteEntera (n-1))
              | otherwise = 0


{-
Ejercicio 3:
Escribir una funcion para determinar si un numero natural es multiplo de 3. 
No esta permitido utilizar mod ni div.
-}
multCheck :: Int -> Int -> Bool
multCheck a b | a == 0 = True
              | a < 0 = False
              | otherwise = (multCheck (a-b) b)

{-
Ejercicio 4:
Implementar la funcion sumaImpares :: Int -> Int que dado n ∈ N sume los primeros n
numeros impares
-}
sumaImpares :: Int -> Int
sumaImpares n | n > 1 = (2*n - 1) + (sumaImpares (n-1))
              | otherwise = 1

{-
Ejercicio 5:
Escribir una funcion medioFact que dado n ∈ N calcula n!! = n (n − 2)(n − 4) · · -
-}
medioFact :: Int -> Int
medioFact n | n == 0 = 1
            | n == 1 = 1
            | otherwise = n * (medioFact (n-2))

{-
Ejercicio 6:
Escribir una funcion que determine la suma de dıgitos de un numero positivo.
-}
sumDigits :: Int -> Int
sumDigits n | divRsl > 0 = mod n 10 + (sumDigits divRsl)
            | otherwise = n
            where divRsl = div n 10

{-
Ejercicio 7:
Implementar una funcion que determine si todos los dıgitos de un numero son iguales.
-}
sameDigits :: Int -> Bool
sameDigits n | divRsl > 0 = ((mod n 10) == (mod divRsl 10)) && sameDigits divRsl 
             | otherwise = True
             where divRsl = div n 10