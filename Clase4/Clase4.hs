
--fromIntegral: Usar esto para los errores de Tipo con Int



factorial :: Int -> Int
factorial n | n == 0 = 1
            | otherwise = n * factorial (n-1)


eAprox :: Int -> Float
eAprox n | n == 0 = 1.0
         | otherwise = 1/fromIntegral(factorial n) + (eAprox (n-1))

eNumero = eAprox 10 