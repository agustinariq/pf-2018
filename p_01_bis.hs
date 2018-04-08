data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
-- arbol de prueba
num = (NodeT 1 (NodeT 1 (NodeT 3 EmptyT EmptyT) EmptyT) (NodeT 2 EmptyT EmptyT))
pal = (NodeT "miku" (NodeT "rin" (NodeT "len" EmptyT EmptyT) (NodeT "megpoid" EmptyT EmptyT)) (NodeT "luka" EmptyT EmptyT))
hei = (NodeT 1 EmptyT (NodeT 3 EmptyT EmptyT))
-- 1.
sumT :: Tree Int -> Int
sumT EmptyT = 0
sumT (NodeT x t1 t2) = x + sumT t1 + sumT t2

-- 2.
sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT x t1 t2) = 1 + sizeT t1 + sizeT t2

-- 3.
mapDoubleT :: Tree Int -> Tree Int
mapDoubleT EmptyT = EmptyT
mapDoubleT (NodeT x t1 t2) = NodeT (x * 2) (mapDoubleT t1) (mapDoubleT t2)

-- 4.
mapLengthT :: Tree String -> Tree Int
mapLengthT EmptyT = EmptyT
mapLengthT (NodeT x t1 t2) = NodeT (length x) (mapLengthT t1) (mapLengthT t2)

-- 5.
elemT :: Eq a => a -> Tree a -> Bool
elemT x EmptyT = False
elemT x (NodeT y t1 t2) = if x == y
                          then True
                          else (elemT x t1) || (elemT x t2)

-- 6.
occurrsT :: Eq a => a -> Tree a -> Int
occurrsT e EmptyT = 0
occurrsT e (NodeT x t1 t2) = if e == x
                          then 1 + (occurrsT e t1) + (occurrsT e t2)
                          else (occurrsT e t1) + (occurrsT e t2)

-- 7. averageT :: Tree Persona -> Int
-- fiaca

-- 8.
isEmptyT:: Tree a -> Bool
isEmptyT EmptyT = True
isEmptyT t = False

isLeaf:: Tree a -> Tree a -> Bool
isLeaf EmptyT EmptyT = True
isLeaf _ _ = False

countLeaves :: Tree a -> Int
countLeaves EmptyT = 0
countLeaves (NodeT x t1 t2) = if isEmptyT t1 && isEmptyT t2
                              then 1 + countLeaves t1 + countLeaves t2
                              else countLeaves t1 + countLeaves t2
-- 9.
leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT x t1 t2) = if isEmptyT t1 && isEmptyT t2
                         then x : leaves t1
                         else leaves t1 ++ leaves t2
-- 10.
heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x t1 t2) = max (1 + (heightT t1)) (1 + (heightT t2))

-- 11.
countNoLeaves :: Tree a -> Int
countNoLeaves EmptyT = 0
countNoLeaves (NodeT x t1 t2) = if isLeaf t1 t2
                                then countNoLeaves t1 + countNoLeaves t2
                                else 1 + countNoLeaves t1 + countNoLeaves t2

-- 12. mirrorT :: Tree a -> Tree a
-- Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho,
-- en cada nodo del árbol.
-- 13. listInOrder :: Tree a -> [a]
-- Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
-- Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
-- y luego los elementos del hijo derecho.
-- 14. listPreOrder :: Tree a -> [a]
-- Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo pre-order.
-- Nota: En el modo pre-order primero se procesa la raiz, luego los elementos del hijo izquierdo,
-- a continuación los elementos del hijo derecho.
-- 15. listPosOrder :: Tree a -> [a]
-- Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo post-
-- order.
-- Nota: En el modo post-order primero se procesan los elementos del hijo izquierdo, a conti-
-- nuación los elementos del hijo derecho y finalmente la raiz.
-- 16. concatT :: Tree [a] -> [a]
-- Dado un árbol de listas devuelve la concatenación de todas esas listas. El recorrido debe ser
-- in-order.
-- 17. levelN :: Int -> Tree a -> [a]
-- Dados un número n y un árbol devuelve una lista con los nodos de nivel n.
-- Nota: El primer nivel de un árbol (su raíz) es 0.
-- 18. listPerLevel :: Tree a -> [[a]]
-- Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de
-- dicho árbol.
-- 19. widthT :: Tree a -> Int
-- Dado un árbol devuelve su ancho (width en inglés), que es la cantidad de nodos del nivel
-- con mayor cantidad de nodos.
-- 20. leftBranches :: Tree a -> [a]
-- Devuelve todos los elementos encontrados en el camino de todas las ramas derechas.
-- 21. longestBranch :: Tree a -> [a]
-- Devuelve los elementos de la rama más larga del árbol
-- 22. allPaths :: Tree a -> [[a]]
-- Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raiz hasta las hojas.
