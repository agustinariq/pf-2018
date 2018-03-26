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
head' [] = error "La lista es vacia"
head' (x:xs) = x

tail':: [a] -> [a]
tail' [] = error "La lista es vacia"
tail' (x:xs) = xs

sum', product' :: Num a => [a] -> a

sum' [] = 0
sum' (x:xs) = x + sum xs

product' [] = 1
product' (x:xs) = x * product' xs

elem', notElem' :: Eq a => a -> [a] -> Bool

elem' x [] = False
elem' x (y:ys) = x == y || elem' x ys

notElem' x [] = False
notElem' x (y:ys) = x /= y || notElem' x ys

and', or' :: [Bool] -> Bool

and' [] = True
and' (x:xs) = x && and' xs

or [] = False
or' (x:xs) = x || or' xs

last' :: [a] -> a
last' [] = error "La lista es vacia"
last' [x] = x
last' (x:xs) = last' xs


init' :: [a] -> [a]
init' [] = error "La lista es vacia"
init' [x] = []
init' (x:xs) = x:(init' xs)


subset' :: Eq a => [a] -> [a] -> Bool
subset' [] ys = True
subset' (x:xs) ys = (elem' x ys) && subset' xs ys

(++-) :: [a] -> [a] -> [a]
(++-) [] ys = ys
(++-) (x:xs) ys = x:(xs ++- ys)


concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ (concat' xss)

(!!!) :: [a] -> Int -> a
(!!!) [] y = error "No existe index"
(!!!) (x:xs) 0 = x
(!!!) (x:xs) y = (!!!) xs (y-1)

take' :: Int -> [a] -> [a]
take' 0 (y:ys) = []
take' x [] = []
take' x (y:ys) = y:(take' (x-1) ys)


drop' :: Int -> [a] -> [a]
drop' 0 ys = ys
drop' x [] = []
drop' x (y:ys) = (drop' (x-1) ys)

zip' :: [a] -> [b] -> [(a,b)]
zip' xs [] = []
zip' [] ys = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' 0 ys = ([], ys)
splitAt' x [] = ([], [])
splitAt' x (y:ys) =  let (list1, list2) = splitAt' (x-1) ys
                         in (y:list1, list2)

maximum', minimum' :: Ord a => [a] -> a

maximum' [] = error "No hay elementos"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

minimum' [] = error "No hay elementos"
minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs)

data Maybe' a = Just' a | Nothing'

lookup' :: Eq a => a -> [(a,b)] -> Maybe' b
lookup' x [] = Nothing'
lookup' x (y:ys) = if x == fst y
                   then Just' (snd y)
                   else lookup' x ys

unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([], [])
unzip' (x:xs) = let (list1, list2) = unzip' xs
                    in ((fst x):list1, (snd x):list2)

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' (x:xs) = (x:xs):tails' xs

replicate' :: Int -> a -> [a]
replicate' 0 y = []
replicate' x y = y: replicate' (x-1) y

repeat' :: a -> [a]
repeat' x = x:repeat' x

cycle' :: [a] -> [a]
cycle' xs = xs ++ (cycle' xs)

nats' :: [Int]
nats' = [1..]

agrupar :: Eq a => [a] -> [[a]]
agrupar [] = [[]]
agrupar [x] = [[x]]
agrupar (x:xs) = let (ys:yss) = agrupar xs
                 in if elem x ys
					then ((x:ys):yss)
					else ([x]:(ys:yss))
-----------------------------------
-- 2. Tipo a expresiones
---- 1.Dar tipo a las siguientes expresiones y funciones
-- a) True::Bool
-- b) [2]::[Int]
-- ?????      c) Maybe ["Jorge"]:: ????
-- d ) Nothing :: Maybe a
-- e) [] :: [a]
-- f ) let x = [] in x ++ x :: [a]
-- ?????      g) let f x = f x in f [] :: [a] -> b ?????????
-- h) data Either a b = Left a | Right b
-- x = Left True :: Either True b
-- y = Right (Left []) :: Either a (Either [b] c)
-- z = Right (Left [Right []]) :: Either a (Eiter [Either c [d]] b)
-- i ) (:) :: a -> [a] -> [a]
-- ?????     j ) Maybe :: a -> Maybe a
-- k ) Right :: b -> Either a b
-- l ) (1:) :: [Int] -> [Int]
-- m) error "ups" :: a
-----
-- Hoogle:
--- error :: [Char] -> a
--- error stops execution and displays an error message.
---
--- undefined ==> A special case of error.
-----
-- n) error :: [Char] -> a
-- ñ) undefined :: a
-- ?????     o) undefined undefined :: a
-- 2. Dar ejemplos de expresiones que posean los siguientes tipos:
-- a) Bool ==> True, False
-- b) (Int, Int) ==> (1,2), (3,4)
-- c) Int -> Int -> Int ==> Suma1:: Int -> Int
-- d ) a -> a ==> id:: a -> a
-- ?????         e) a ==> error
-- f ) String -> a
-- g) a -> b

--3. Patterns
-- ???????????
-- 5.
-- Terminación
-- Indicar qué programas terminan
-- 1. let nats = [1..] in nats
---- No termina.
-- 2. take 5 [1..]
---- Termina porque haskell usa lazy evaluation.
-- 3. let appendedNats = [1..] ++ [1..] in take 5 appendedNats
---- Termina por la misma razon del 2.
-- 4. let x = x in x
---- No termina porque se redefine hasta el infinito??
-- 5. undefined
-- 6. undefined undefined
---- deberia terminar si undefined es un caso de error ??
