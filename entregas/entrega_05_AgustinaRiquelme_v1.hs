-- Entrega 05 (foldr)
-- Agustina Riquelme
-- 2.2.
-- Definir para listas:
-- 1.
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f [] = error "La lista es vacia"
foldr1' f [x] = x
foldr1' f (x:xs) = f x (foldr1' f xs)
-- 2.
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

-- 2.3.
-- Definir todas las funciones ya definidas sobre listas foldr, foldr1 o recr (dependiendo del
-- caso), y sobre árboles binarios con foldT. Tener en cuenta que en algunos casos pueden ser difíciles,
-- o que no se puedan realizar con recursión estructural.

head' :: [a] -> a
head' = foldr1' (\ x r -> x)

tail' :: [a] -> [a]
tail' = recr (\ x xs r -> xs) (error "La lista es vacia")

null' :: [a] -> Bool
null' = foldr (\ x r -> False) True

sum', product' :: Num a => [a] -> a

sum' = foldr (\ x r -> x + r) 0

product' = foldr (\ x r -> x * r) 1

length' :: [a] -> Int
length' = foldr (\ x r -> 1 + r) 0

elem' :: Eq a => a -> [a] -> Bool
elem' y xs = foldr (\ x r -> x == y || r ) False xs

and', or' :: [Bool] -> Bool

and' = foldr (\ x r -> x && r) True

or' = foldr (\ x r -> x || r) False

-- last' :: [a] -> a
-- last' [] = error "La lista es vacia"
-- last' [x] = x
-- last' (x:xs) = last' xs

last' :: [a] -> a
last' = foldr1' (\ x r -> r)

-- init' :: [a] -> [a]
-- init' [] = error "La lista es vacia"
-- init' [x] = []
-- init' (x:xs) = x:(init' xs)

-- no me salio
--init' :: [a] -> [a]
--init' xs =  recr (\ x xs r -> [1]) (lastOrError) where lastOrError xs = if null xs then error "Lista es vacia" else head xs

-- snoc no es recursivo
--snocc e [] = [e]
--snocc e xs = xs ++ [e]

snoc' :: [a] -> a -> [a]
snoc' xs e = recr (\ x xs r -> (x:xs) ++ [e] ) [e] xs

--reverse'' [] = []
--reverse'' (x:xs) = reverse'' xs ++ [x]

reverse' :: [a] -> [a]
reverse' = foldr (\ x r -> r ++ [x]) []

-- subset' :: Eq a => [a] -> [a] -> Bool
-- subset' [] ys = True
-- subset' (x:xs) ys = (elem' x ys) && subset' xs ys

subset' :: Eq a => [a] -> [a] -> Bool
subset' xs ys = foldr (\ x r -> elem x ys && r) True xs

(+++) :: [a] -> [a] -> [a]
(+++) xs ys = foldr (\ x r -> x:r) ys xs

concat' :: [[a]] -> [a]
concat' = foldr (\ x r -> x ++ r) []
