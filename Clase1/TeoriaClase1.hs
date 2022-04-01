doble x = 2 * x
suma x y = x + y
normaVectorial x1 x2 = sqrt(x1**2 + x2**2)
funcionConstante x = 8

signo n | n < 0 = -1
        | n == 0 = 0
        | n > 0 = 1

outOfRange n | n > 3 = undefined

--Dados dos n ́umeros b y c, calcula la cantidad de soluciones reales la ecuacion cuadratica
--X**2 + bX + c
cantidadDeSoluciones b c | (b**2 - 4*c) > 0 = 2
                         | (b**2 - 4*c) == 0 = 1
                         | otherwise = 0

cantidadDeSolucionesV2 b c = 1 + (signo (b**2 - 4*c))

maxRac :: Float -> Float -> Float
maxRac x y | x >= y = x
           | otherwise = y

funcionRara :: Float -> Float -> Bool -> Bool
funcionRara _ _ True = True
funcionRara x y False = x >= y
