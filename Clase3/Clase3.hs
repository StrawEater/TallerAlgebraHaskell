llenarLinea n | n > 0 = (putStr "d") >> (llenarLinea (n-1))
                | otherwise = (putStrLn "dou")


prueba:: Int -> Float -> Float -> IO()
prueba n thi max | n > 0 = (llenarLinea length) >> (prueba (n-1) (thi + 0.1) max)
                 | otherwise = (llenarLinea n)
                 where length = (round (max * ((cos thi)+1)))

 