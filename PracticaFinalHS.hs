--Autor: Alejandro Daniel Díaz Román
--Asignatura: Programación Funcional (UCM)
--Fecha: 17/01/2020

data Vertice = A|B|C|D|E|F deriving (Read, Show, Eq)
data Grafo = G [Vertice] [(Vertice, Vertice)] deriving (Read, Show)

--Permite comparar si dos grafos son isomorfos
instance Eq Grafo where
    (G a1 a2) == (G b1 b2) = isomorfos (G a1 a2) (G b1 b2)
 
{-
Declaración de los grafos
-}

g1 = G [B,D,E,C] [(D,E),(E,B),(E,C),(C,B)]
g2 = G [D,F,E] [(D,F),(E,D),(D,E),(F,E)]
g3 = G [A,C,D] [(A,C), (C,D), (A,D)]
g4 = G [A,D,E] [(A,D),(E,D),(E,A),(D,E)]
g5 = G [A,B,C,E,F] [(E,A),(C,F),(B,A),(E,F),(E,C),(F,A)]
g6 = G [D,E,F,A] [(F,A),(F,E),(F,D),(A,D)]
g7 = G [D,E,F,A] [(F,A),(F,A),(F,D),(A,D)]

--g2 y g4 son isomorfos
--Se puede realizar la comparación mediante g2 == g4



{-

SECCIÓN PARA COMPROBAR SI DOS GRAFOS SON ISOMORFOS

-}



--Introduce dos grafos y devuelve si son isomorfos o no
isomorfos :: Grafo -> Grafo -> Bool
isomorfos (G x1 y1) (G x2 y2) = comparaGradosNeg (G x1 y1) (G x2 y2) && comparaGradosPos (G x1 y1) (G x2 y2)


--Se encarga de comparar si los grados negativos de dos grafos son iguales
comparaGradosNeg :: Grafo -> Grafo -> Bool
comparaGradosNeg (G x1 y1) (G x2 y2) = extraeGradosNegYOrdena (G x1 y1) == extraeGradosNegYOrdena (G x2 y2)


--Se encarga de comparar si los grados positivos de dos grafos son iguales
comparaGradosPos :: Grafo -> Grafo -> Bool
comparaGradosPos (G x1 y1) (G x2 y2) = extraeGradosPosYOrdena (G x1 y1) == extraeGradosPosYOrdena (G x2 y2)


--	 Como indica el nombre, extrae los grados negativos ayudándose de unzip ya que la función
-- grados_neg devuelve una lista de tuplas [(Vertice, Int)] donde el 'Int' determina el grado
extraeGradosNegYOrdena :: Grafo -> [Int]
extraeGradosNegYOrdena (G x y) = quicksort $ snd $ unzip $ grados_neg (G x y)


--Igual que la función 'extraeGradosNegYOrdena' pero con los grados positivos
extraeGradosPosYOrdena :: Grafo -> [Int]
extraeGradosPosYOrdena (G x y) = quicksort $ snd $ unzip $ grados_pos (G x y)


--Gracias a la Wiki de Haskell: https://wiki.haskell.org/Introduction#Quicksort_in_Haskell
quicksort :: (Ord a) => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs




{-

SECCIÓN PARA COMPROBAR SI UN GRAFO ES VÁLIDO

-}




--Comprueba si todos los los elementos de una lista son diferentes
diferentes :: (Eq a) => [a] -> Bool
diferentes [] = True
diferentes (x:xs) = x `notElem` xs && diferentes xs

--Comprueba si todos los los elementos de una lista de tuplas son diferentes
tuplasDiferentes :: (Eq a) => [(a,a)] -> Bool
tuplasDiferentes [] = True
tuplasDiferentes (x:xs) = x `notElem` xs && tuplasDiferentes xs

--Comprueba si los elementos de una lista de tuplas están en una lista
--Usada principalmente para la función 'es_grafo', pero podría tener otras utilidades
tuplaEnLista :: (Eq a) => [a] -> [(a,a)] -> Bool
tuplaEnLista [] [] = True
tuplaEnLista (x:xs) [] = True
tuplaEnLista (x:xs) (y:ys) = fst(y) `elem` (x:xs) && snd(y) `elem` (x:xs) && tuplaEnLista (x:xs) (ys)


--Para comprobar si un grafo es válido tiene en cuenta varios casos:
--	Si no hay nodos no es válido
--	Si hay nodos (y son diferentes) es válido aunque no haya aristas que los conecten
--	Si hay nodos (y son diferentes) y los nodos que unen las aristas están en la lista de nodos y son diferentes
es_grafo :: Grafo -> Bool
es_grafo (G [] _) = False
es_grafo (G (x:xs) []) = diferentes (x:xs)
es_grafo (G (x:xs) (y:ys)) = diferentes (x:xs) && tuplaEnLista (x:xs) (y:ys) && tuplasDiferentes (y:ys)




{-

SECCIÓN PARA GENERAR LA MATRIZ DE ADYACENCIA

-}



--Crea la matriz de adyacencia en base a una lista de nodos y una lista de aristas que conectan nodos
crearMatriz :: (Eq a) => [a] -> [(a,a)] -> [[Int]]
crearMatriz [] _ = [[]]
crearMatriz (xs) (ys) = [ [ if (x,y) `elem` ys then 1 else 0 | y <- xs] | x <- xs]


--Función principal para generar la matriz de adyacencia
mat_ady :: Grafo -> [[Int]]
mat_ady (G [] _) = [[]]
mat_ady (G (xs) (ys)) = crearMatriz xs ys




{-

SECCIÓN PARA CALCULAR LOS GRADOS (POSITIVOS Y NEGATIVOS) DEL GRAFO

-}




--Recibe dos listas de nodos, la primera son todos los nodos que hay y la segunda servirá para
--contar el número de veces que aparece, lo cual indicará el grado de ese nodo
calcularGrado :: (Eq a) => [a] -> [a] -> [(a, Int)]
calcularGrado (x:xs) (l:ls) = [ (a, length (filter (==a) (l:ls))) | a <- (x:xs)]


--Calcula los grados positivos de los nodos del grafo, es decir el número de aristas que "salen" de cada nodo
grados_pos :: Grafo -> [(Vertice,Int)]
grados_pos (G _ []) = []
grados_pos (G (x:xs) (y:ys)) = calcularGrado (x:xs) (fst(unzip(y:ys)))


--Calcula los grados negativos de los nodos del grafo, es decir el número de aristas que "entran" a cada nodo
grados_neg :: Grafo -> [(Vertice, Int)]
grados_neg (G _ []) = []
grados_neg (G (x:xs) (y:ys)) = calcularGrado (x:xs) (snd(unzip(y:ys)))




{-

SECCIÓN PARA LA FUNCIÓN 'camino_lng'

-}



--Trabajo en progreso de la función camino_lng
camino_lng :: Grafo -> Vertice -> Int -> [[Vertice]]
camino_lng (G x y) v i
                      | i <= 0 = []
					  
					  
					  
					  
{-

SECCIÓN PARA COMPROBAR SI UN GRAFO ES CONEXO

-}



--Recibe dos listas y comprueba si todos los elementos de la primera están en la segunda
contiene :: (Eq a) => [a] -> [a] -> Bool
contiene [] ys = True
contiene (x:xs) y = elem x y && contiene xs y


--Comprueba si dos listas contienen los mismos elementos comprobando si los elementos de una están en la otra y viceversa
arraysIguales :: (Eq a) => [a] -> [a] -> Bool
arraysIguales x y = contiene x y && contiene y x


--Comprueba si un vértice está conectado al resto de vértices del grafo.
--Para ello comprobará si la lista de nodos del grafo es igual al recorrido en anchura del grafo desde
-- el vértice v.
verticeConectado :: Vertice -> Grafo -> Bool
verticeConectado v (G x y) = arraysIguales x (bfsAux v (G x y))


--Comprueba si el grafo es conexo, para ello analizará si cada vértice está conectado al resto de vértices
esConexo :: Grafo -> [Vertice] -> Bool
esConexo (G x y) [] = True
esConexo (G x y) (z:zs) = (verticeConectado z (G x y)) && (esConexo (G x y) zs)


--Función principal para comprobar si un grafo es conexo
conexo :: Grafo -> Bool
conexo (G x y) = esConexo (G x y) x


----Función auxiliar para ejecutar el algoritmo de primero en anchura sobre un grafo
bfsAux :: Vertice -> Grafo -> [Vertice]
bfsAux v (G (x:xs) y) = bfs (G (x:xs) y) [v] [v] v


--Algoritmo de primero en anchura con control de repetidos
--Gracias al control de repetidos evitamos entrar en bucles infinitos y así poder acabar el BFS
--      Grafo -  Cola -   Visitados -   V inicial -     Out
bfs :: Grafo -> [Vertice] -> [Vertice] -> Vertice ->[Vertice]
bfs _ [] z _ = z
bfs (G x y) (c:cs) z v = bfs (G x y) cola visitados $ head cola
    where h = hijosDeVertice (G x y) v
          p = anadeLosNuevos z h
          cola = cs ++ p
          visitados = z ++ p


--Devuelve los vértices a los que está conectado un vértice (sus hijos) 
hijosDeVertice :: Grafo -> Vertice -> [Vertice]
hijosDeVertice (G x y) v = hijoMatHelper (G x y) v x (mat_ady (G x y))


--Para calcular los vértices hijos de un nodo se usará la matriz de adyacencia.
-- Recorre los nodos del grafo y cuando el que está buscando coincide con nuestro vértice
-- indicará la fila de la matriz de adyacencia, de la cual se extraen los nodos que están conectados en 'extraeVertices'
hijoMatHelper :: Grafo -> Vertice -> [Vertice] -> [[Int]] -> [Vertice]
hijoMatHelper (G x y) v [] [] = []
hijoMatHelper (G (x:xs) y) v (a:al) (z:zs) 
                               | v == a = extraeVertices (G (x:xs) y) z
                               | v /= a = hijoMatHelper (G (x:xs) y) v al zs


--A través de una de las filas de la matriz de adyacencia si el contenido es un 0 no añade el nodo
-- correspondiente a la lista, por el contrario si es un 1, lo añade
extraeVertices :: Grafo -> [Int] -> [Vertice]
extraeVertices (G [] y) _ = []
extraeVertices (G (x:xs) y) (z:zs) 
                              | z == 1 = [x] ++ extraeVertices (G xs y) zs
                              | z == 0 = extraeVertices (G xs y) zs


--Elimina los elementos repetidos de una lista
eliminarRepetidos :: (Eq a) => [a] -> [a]  
eliminarRepetidos [] = []
eliminarRepetidos (x:xs)
                    | elem x xs = eliminarRepetidos xs
                    | otherwise = x:eliminarRepetidos xs


--Recibe dos listas. Devuelve los elementos de la segunda que no estén en la primera
anadeLosNuevos :: (Eq a) => [a] -> [a] -> [a]
anadeLosNuevos [] [] = []
anadeLosNuevos [] a = a
anadeLosNuevos a [] = []
anadeLosNuevos x (y:ys)
                   | elem y x = anadeLosNuevos x ys
                   | otherwise = y:anadeLosNuevos x ys