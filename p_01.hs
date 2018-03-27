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

-- 6.Typeclasses
-- 6.1.Prelude
-- Describa el propósito de las siguientes typeclasses:
-- Enum: define funciones que describen el comportamiento de tipos ordenables
--       secuencialmente. Por ejemplo, define succ y pred.
-- Ord: define funciones que describen el comportamiento de tipos ordenables
--      comparativamente (?). Dados dos elementos de tipo ord, se puede saber
--      cual es mas grande o mas chico de los dos utilizando operaciones como:
--       >, >=, min, max, etc.
-- Eq: define la igualdad y la desigualdad para los distintos tipos
-- Bounded
-- como que indica el minimo y em maximo de un tipo???
-- Show
-- convierte valores en string legibles.
-- Read
-- parsea string convirtiendolo en valores
-- Num
-- clase basica de numeros. Define suma, resta, valor absoluto, etc.
--
-- 6.2.Tipado
-- Indicar el tipo y el resultado de las siguientes expresiones:
-- 1. 5 :: Num a => a
-- 2. 2.0 :: Fractional a => a
-- 3. (5, 2.0) :: (Fractional a, Num b) => (b,a)
-- 4. 5 + 2.0 :: Fractional a => a
-- 5. minBound :: Bounded a => a
-- 6. minBound && True :: Bool    !!!!
-- 7. succ :: Enum a => a -> a
-- 8. succ False :: Bool
-- 9. succ True :: Bool
-- 10. succ [] ::  no tipa ???
-- 11. succ ’a’ :: Char
-- 12. read "5" :: Read a => a
-- 13. read "5" :: Int    ---> ::Int
-- 14. let xs = [1,2,3] in xs :: Num a => [a]
-- 15. Dado xs = [1,2,3] en un archivo, xs ??????? ???
-- 6.3 Implementacion y uso
--
-- Lo dejo sin hacer.
--
