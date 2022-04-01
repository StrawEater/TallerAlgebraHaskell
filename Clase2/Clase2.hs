{-
Segunda clase 2
-}

identiidad x = x

identidadInt :: Int -> Int
identidadInt x = x

-- El tipo va a Cambiar segun el tipo de X
-- Si x = Bool => t = Bool
-- Si x = Int => t = Int
-- Etc
identidadDinamica :: t -> t
identidadDinamica x = x

masTipos :: tx -> ty -> tx
masTipos x y = x

-- X y Y tienen que tener el mismo tipo o da error
mismoTipo :: t -> t -> Int
mismoTipo x y = 4

-- False <= True

-- 3**(Numero Real)
-- 3 ^ (Numero Entero)

-- Cualquier tipo t que pueda multiplicarse
triple :: (Num t) => t -> t
triple t = t*3

-- :t funcion para conocer el tipo

--Haskell solo puede devolver un tipo de dato


dou x | x < 10 = dou (x+1)
      | otherwise = x

--(+) 4 3 = 4 + 3
-- div 4 5 = 4 `div` 5

--TUPLAS Y COSAS
