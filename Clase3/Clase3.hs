llenarLinea n | n > 0 = (putStr "-") >> (llenarLinea (n-1))
                | otherwise = (putStrLn "")


prueba:: Int -> Float -> Float -> IO()
prueba n thi max | n > 0 = (llenarLinea length) >> (prueba (n-1) (thi + 0.1) max)
                 | otherwise = (llenarLinea n)
                 where length = (round (max * ((cos thi)+1)))


-- GRAFICA MEJORADA
-----------------------------------------------------------------
-- Poner Igualdad aca
-- La esquina superior izquierda es el (0,0)
getFull:: Float -> Float -> Bool
--getFull x y = y >= ((x-25)**2)/10
{-
getFull x y | y >= ((x-25)**2)/10 = True
            | (y*2 - 90)**2 + (x - 5)**2 <= (15)**2 = True
            | (y*2 - 90)**2 + (x - 50)**2 <= (15)**2 = True
            | otherwise = False
-}
--getFull x y = x <= ((cos y) + 1) * 20
getFull x y = (xC**2 + yC**2 - 2*(a)*xC)**2 <= 4*((a)**2)*(xC**2 + yC**2)
              where 
                  xC = x - 5
                  yC = y - 35
                  a = 10

-- Escribe un "0" si el punto esta dentro del area, " " si no
lineaGraf:: Float -> Float -> Float -> IO ()
lineaGraf n y xMax | (n < xMax && (getFull n y)) = (putStr "0") >> (lineaGraf (n+1) y xMax)
                   | (n < xMax && (not (getFull n y))) = (putStr " ") >> (lineaGraf (n+1) y xMax)
                   | otherwise = (putStrLn "")

-- Llamar a esta funcion para Graficar
graf:: Float -> Float -> Float -> Float -> IO ()
graf xMax yMax y ySpeed | yMax > y = (lineaGraf 0 y xMax) >> (graf xMax yMax (y+ySpeed) ySpeed)
                        | otherwise = (llenarLinea xMax)
                 


 
