
{-Ejercicio 1
Funcion que devuelva la suma de los divisores de un numero hasta cierto punto.
-}
sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta a b | b == 1 = 1
                       | mod a b == 0 = b + divisorAnterior
                       | otherwise  = divisorAnterior
                       where
                           divisorAnterior = sumaDivisoresHasta a ( b - 1)

{-Ejercicio 2 
sumaDivisores :: Int -> Int que calcule la suma de los divisores un entero positivo.
-}
sumaDivisores :: Int -> Int
sumaDivisores a = sumaDivisoresHasta a a

{-Ejercicio 3
Implementar menorDivisor :: Int -> Int que calcule el menor divisor (mayor que 1) de
un natural n
-}
menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde a b | b > a = 0
                      | mod a b == 0 = b
                      | otherwise = menorDivisorDesde a (b+1)

menorDivisor :: Int -> Int
menorDivisor a = menorDivisorDesde a 2

---------------------------------------------------

{-Ejercicio 4
Implementar la funcion esPrimo :: Int -> Bool 
-}
esPrimo :: Int -> Bool
esPrimo a = (menorDivisor a) == a


{-Ejercicio 5
Implementar la funci ́on nEsimoPrimo :: Int -> Int que devuelve el n-esimo primo
(n ≥ 1, el primer primo es el 2, el segundo es el 3, el tercero es el 5, etc.)
-}
nEsimoPrimoDesde :: Int -> Int -> Int
nEsimoPrimoDesde a b | a == 0 = (b - 1)
                     | esPrimo b = nEsimoPrimoDesde (a - 1) (b + 1)
                     | otherwise = nEsimoPrimoDesde a (b + 1)

nEnesimoPrimo :: Int -> Int
nEnesimoPrimo a = nEsimoPrimoDesde a 2

----------------------

{-Ejercicio 6
Implementar menorFactDesde :: Int -> Int que dado m ≥ 1 encuentra el m ́ınimo n ≥ m tal que n = k! para alg ́un k.
7 Implementar mayorFactHasta :: Int -> Int que dado m ≥ 1 encuentra el maximo
n ≤ m tal que n = k! para alg ́un k.
8 Implementar esFact :: Int -> Bool que dado n ≥ 0 decide si existe un n ́umero entero
k ≥ 0 tal que n = k!
E

-}
factorial :: Int -> Int
factorial n | n == 0 = 1
            | otherwise = n * factorial (n-1)

esFactorialHasta :: Int -> Int -> Bool
esFactorialHasta a b | factorial b == a = True 
                     | b == 0 = False
                     | otherwise = esFactorialHasta a (b - 1)

esFactorial :: Int -> Bool 
esFactorial a = esFactorialHasta a a

menorFactDesde :: Int -> Int 
menorFactDesde a | esFactorial a = a
                 | otherwise = menorFactDesde (a + 1)

mayorFactHasta :: Int -> Int 
mayorFactHasta a | esFactorial a = a
                 | otherwise = mayorFactHasta (a - 1)                 

----------------------------------------------

nEsimoFibonacci :: Int -> Int 
nEsimoFibonacci a | a == 1 = 1
                  | a == 2 = 1
                  | otherwise = nEsimoFibonacci (a - 1) + nEsimoFibonacci (a - 2) 

esFibonacciDesde :: Int -> Int -> Bool 
esFibonacciDesde a b | b == 0 = False 
                     | a == nEsimoFibonacci b = True 
                     | otherwise = esFibonacciDesde a (b-1) 

esFibonacci :: Int -> Bool 
esFibonacci a = esFibonacciDesde a a

-----------------

sumaDePrimerosPrimos :: Int -> Int 
sumaDePrimerosPrimos a | a == 0 = 0
                       | otherwise = nEnesimoPrimo a + sumaDePrimerosPrimos (a - 1) 

esSumaDePrimerosPrimosHasta :: Int -> Int -> Bool                        
esSumaDePrimerosPrimosHasta a b | b == 0 = False 
                                | a == sumaDePrimerosPrimos b = True
                                | otherwise = esSumaDePrimerosPrimosHasta a (b - 1) 

esSumaDePrimerosPrimos :: Int -> Bool 
esSumaDePrimerosPrimos a = esSumaDePrimerosPrimosHasta a a

-------------