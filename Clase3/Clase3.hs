llenarLinea n | n > 0 = (putStr "d") >> (llenarLinea (n-1))
                | otherwise = (putStrLn "")


prueba:: Int -> Float -> Float -> IO()
prueba n thi max | n > 0 = (llenarLinea length) >> (prueba (n-1) (thi + 0.1) max)
                 | otherwise = (llenarLinea n)
                 where length = (round (max * ((cos thi)+1)))


--Poner Igualdad aca
getFull:: Float -> Float -> Bool
--getFull x y = y >= ((x-25)**2)/10
--getFull x y = x <= ((cos y) + 1) * 20
getFull x y = (y - 20)**2 + (x - 20)**2 <= (15)**2 

--
lineaGraf:: Float -> Float -> Float -> IO ()
lineaGraf n y xMax | (n < xMax && (getFull n y)) = (putStr "d") >> (lineaGraf (n+1) y xMax)
                   | (n < xMax && (not (getFull n y))) = (putStr " ") >> (lineaGraf (n+1) y xMax)
                   | otherwise = (putStrLn "")

--Llamar a esta funcion para Graficar
graf:: Float -> Float -> Float -> IO ()
graf xMax yMax y | yMax > y = (lineaGraf 0 y xMax) >> (graf xMax yMax (y+2))
                 | otherwise = (llenarLinea xMax)
                 
