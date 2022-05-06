-- Creamos un tipo llamado Set, en terminos practicos funciona como una lista, pero nosotros lo trataremos como un Conjunto.
-- Nosotros tenemos que hacernos cargo de que se trate como un Conjunto
type Set a = [a]

-- Devuelve True si el Elemento esta dentro del Conjunto
formaParte :: Int -> Set Int -> Bool
formaParte _ [] = False
formaParte n (x:xs) | n == x = True
                    | otherwise = formaParte n xs

-- elem -> te da si el elemento esta adentro

-- Conjunto Vacio
vacio :: Set Int
vacio =  []

-- Agrega el Elemento si este no esta ya dentro del Conjunto
agregar ::  Int -> Set Int -> Set Int
agregar n st | formaParte n st == False = n : st
             | otherwise = st

-- Determina si el primer conjunto esta dentro del segundo
incluido :: Set Int -> Set Int -> Bool
incluido [] st2 = True
incluido (x : xs) st2 | formaParte x st2 == True = incluido xs st2
                      | otherwise = False

-- Determina si ambos conjuntos son iguales
iguales :: Set Int -> Set Int -> Bool
iguales st1 st2 = (incluido st1 st2) && (incluido st2 st1) 

-----------------
--Devuelve una lista de listas con st2 mas un elemento de la primera lista
--[1,2,3] [4,5] => [[1,4,5],[2,4,5],[3,4,5]]
agregarElementos :: Set Int -> Set Int -> Set (Set Int)
agregarElementos [] st2 = [st2]
agregarElementos (x:[]) st2 = [x : st2]
agregarElementos (x:xs) st2 = (x : st2) : (agregarElementos xs st2)

-- Concaneta dos Listas de Listas
-- [[1,2],[4,5]] [[3],[5]] => [[1,2],[4,5],[3],[5]]
agregar2 :: Set (Set Int) -> Set(Set Int) -> Set(Set Int)
agregar2 [] st2 = st2
agregar2 (x:xs) st2 = agregar2 xs (x : st2) 

--Dado una lista, da todas las particiones posibles
--[1,2,3] => [[],[1,2,3],[1,2],[1],[2],[3],[1,3],[2,3]]
particiones :: Set Int -> Set (Set Int)
particiones [] = [[]]
particiones (x:[]) = [[x], []]
particiones (x:xs) =  agregar2 (particiones xs) (agregar2 (agregarElementos xs [x]) [[x]])

-- Genera todos los subconjuntos del conjunto [1..n]
partes :: Int -> Set(Set Int)
partes n = particiones [1..n]
-----------------

-- Concaneta dos Listas de Pares
-- [(1,2),(4,5)] [(3,3),(5,8)] => [(1,2),(4,5),(3,3),(5,8)]
agregarPares :: Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
agregarPares [] sp2 = sp2
agregarPares (p:ps) sp2 = agregarPares ps (p : sp2) 

--Devuelve una lista de Pares Formado por cada elemento del primer Set y un Numero
--[1,2,3] 4 => [(1,4),(2,4),(3,4)]
crearPar :: Set Int -> Int -> Set (Int, Int)
crearPar (x:[]) par2 = [(x,par2)]
crearPar (x:xs) par2 = (x,par2) : (crearPar xs par2)

-- Devuelve todos los posibles pares donde el primer elemento pertenece a st1, y el segundo a st2
-- [1,2] [3,4] => [[1,3],[2,3],[1,4],[2,4]]
productoCartesiano :: Set Int -> Set Int -> Set (Int, Int)
productoCartesiano [] _ = []
productoCartesiano _ [] = []
productoCartesiano st1 (t:[]) = crearPar st1 t
productoCartesiano st1 (t:ts) = agregarPares (crearPar st1 t) (productoCartesiano st1 ts)
