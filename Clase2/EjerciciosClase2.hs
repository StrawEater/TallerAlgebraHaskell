--Ejercicio 1
getRelacion :: Float -> Int
getRelacion a | a <= 3 = 0
              | a > 3 && a <= 7 = 1
              | otherwise = 2  

estanRelacionados :: Float -> Float -> Bool
estanRelacionados a b = (getRelacion a) == (getRelacion b)

--Ejercicio 2
prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

--Ejercicio 3
todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (x1, y1) (x2, y2) = x1 > x2 && y1 > y2

--Ejercicio 4
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1, y1) (x2, y2) = sqrt ((x1 - x2)**2 + (y1 - y2)**2)

--Ejercicio 5
sumaTernea :: (Int, Int, Int) -> Int
sumaTernea (a,b,c) = a + b + c

--Ejercicio 6
posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (a, b, c) | mod a 2 == 0 = 1
                         | mod b 2 == 0 = 2
                         | mod c 2 == 0 = 3
                         | otherwise = 4

--Ejercicio 7
crearPar :: a -> b -> (a,b)
crearPar x y = (x,y)

--Ejercicio 8
invertir :: (a,b) -> (b,a)
invertir (x,y) = (y,x)
