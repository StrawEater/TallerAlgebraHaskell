-- Devuelve una tupla con el cociente y resto de a dividio d
sign :: Int -> Int
sign a | a == 0 = 0
       | otherwise = div a (abs a)


division :: Int -> Int -> (Int, Int)
division a d | a < d && a >= 0 = (0,a)
             | otherwise =  (sign (a*d) + cociente, resto)
               where 
                (cociente, resto) = (division (a - diferencia) d)
                diferencia = d * (sign (a*d))


mcd :: Int -> Int -> Int
mcd a b | b == 0 = a
        | otherwise = mcd b resto
        where
            resto = snd (division a b)

emcd :: Int -> Int -> (Int, Int , Int)
emcd a 0 = (a,1,0)
emcd a b = (g, tp, sp-tp*cociente)
        where 
            (cociente, resto) = division a b
            (g, sp, tp) = emcd b resto


{--
(533 48)
533 = 48........





















*11 + 5 (1, -11 , 1)
48 = 5*9 + 3 (1,-9, 1)
5 = 3 + 2 (1,-1, 1)
3 = 2 + 1 0(1, -1, 1)



0

532 = 48*11 + 4
48 = 4*22 + 0
4 = 0 + 4







--}

