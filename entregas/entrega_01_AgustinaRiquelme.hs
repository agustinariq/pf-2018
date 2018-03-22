-- [entrega-01] Agustina Riquelme
-- Programacion funcional
-- Practica 1
--
-- 1. Funciones basicas

id':: a -> a
id' x = x

const':: a -> b -> a
const' x y = x

fst':: (a,b) -> a
fst' (x, y) = x

snd':: (a,b) -> b
snd' (x,y) = y

swap'::(a,b) -> (b,a)
swap' (x, y) = (y, x)

head':: [a] -> a
head' (x:xs) = x
head' [] = error "La lista es vacia"

tail':: [a] -> [a]
tail' (x:xs) = xs
tail' [] = error "La lista es vacia"

sum', product' :: Num a => [a] -> a

sum' (x:xs) = x + sum xs
sum' [] = 0

product' (x:xs) = x * product' xs
product' [] = 1

elem', notElem' :: Eq a => a -> [a] -> Bool

elem' x (y:ys) = x == y || elem' x ys
elem' x [] = False

notElem' x (y:ys) = x /= y || notElem' x ys
notElem' x [] = False

and', or' :: [Bool] -> Bool

and' (x:xs) = x && and' xs
and' [] = True

or' (x:xs) = x || or' xs
or [] = False

last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs
last' [] = error "La lista es vacia"

init' :: [a] -> [a]
init' [x] = []
init' (x:xs) = x:(init' xs)
init' [] = error "La lista es vacia"

subset' :: Eq a => [a] -> [a] -> Bool
subset' (x:xs) ys = (elem' x ys) && subset' xs ys
subset' [] ys = True
subset' [] [] = True

(++-) :: [a] -> [a] -> [a]
(++-) (x:xs) ys = x:(xs ++- ys)
(++-) [] ys = ys

concat' :: [[a]] -> [a]
concat' (xs:xss) = xs ++ (concat' xss)
concat' [] = []

(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x
(!!!) (x:xs) y = (!!!) xs (y-1)
(!!!) [] y = error "No existe index"

take' :: Int -> [a] -> [a]
take' 0 (y:ys) = []
take' x (y:ys) = y:(take' (x-1) ys)
take' x [] = []

drop' :: Int -> [a] -> [a]
drop' 0 ys = ys
drop' x (y:ys) = (drop' (x-1) ys)
drop' x [] = []

zip' :: [a] -> [b] -> [(a,b)]
zip' (x:xs) (y:ys) = (x, y):zip' xs ys
zip' xs [] = []
zip' [] ys = []

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' 0 ys = ([], ys)
splitAt' x [] = ([], [])
splitAt' x (y:ys) =  let (list1, list2) = splitAt' (x-1) ys
                         in (y:list1, list2)

maximum', minimum' :: Ord a => [a] -> a

maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
maximum' [] = error "No hay elementos"

minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs)
minimum' [] = error "No hay elementos"

data Maybe' a = Just' a | Nothing'

lookup' :: Eq a => a -> [(a,b)] -> Maybe' b
lookup' x (y:ys) = if x == fst y
                   then Just' (snd y)
                   else lookup' x ys
lookup' x [] = Nothing'

unzip' :: [(a,b)] -> ([a],[b])
unzip' (x:xs) = (fst x : (fst (unzip xs)), snd x : (snd (unzip xs)) )
unzip' [] = ([], [])

tails' :: [a] -> [[a]]
tails' (x:xs) = (x:xs):tails' xs
tails' [] = [[]]

replicate' :: Int -> a -> [a]
replicate' x y = y: replicate' (x-1) y
replicate' 0 y = []

repeat' :: a -> [a]
repeat' x = x:repeat' x

cycle' :: [a] -> [a]
cycle' xs = xs ++ (cycle' xs)

nats' :: [Int]
nats' = [1..] 

-- auxiliar para agrupar
primerosIguales:: Eq a => a -> [a] -> ([a], [a])
primerosIguales x [] = ([], [])
primerosIguales x (y:ys) = if x == y
					   then let (l1, l2) = primerosIguales x ys
					     in ((y:l1), l2)
					   else ([], y:ys)

agrupar :: Eq a => [a] -> [[a]]
agrupar [] = []
agrupar (x:xs) = let (iguales, resto) = primerosIguales x (x:xs)
                                 in iguales:agrupar resto
