-- [entrega-02] Agustina Riquelme
-- Programacion funcional
-- Practica 2
--
-- 1. Funciones de alto orden
-- a )
apply :: (a -> b) -> a -> b
apply f x = f x

-- b )
twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- c )
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

-- d )
--(.-) :: (b -> c) -> (a -> b) -> (a -> c)
--(.-) f g = g f

-- e)
curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)

-- f )
uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y) = f x y

-- g)
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

-- h)
filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) = if f x
                  then x : filter' f xs
                  else filter' f xs

-- i )
any', all' :: (a -> Bool) -> [a] -> Bool

any' f [] = False
any' f (x:xs) = if f x
               then True
               else any' f xs

all' f [] = True
all' f (x:xs) = if f x
               then True && all' f xs
               else False

-- j )

data Maybe' a = Just' a | Nothing'

maybe' :: b -> (a -> b) -> Maybe' a -> b
maybe' x f (Nothing') = x
maybe' x f (Just' y) = f y

-- k )
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left x) = f x
either' f g (Right x) = g x

-- l )
find' :: (a -> Bool) -> [a] -> Maybe' a
find' f [] = Nothing'
find' f (x:xs) = if f x
                then Just' x
                else find' f xs

-- m)
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' f [] = ([], [])
partition' f (x:xs) = let (list1, list2) = partition' f xs
                      in if f x
                         then (x:list1, list2)
                         else (list1, x:list2)

-- n)
-- no me salio
-- nubBy' :: (a -> a -> Bool) -> [a] -> [a]
-- nubBy' f [] = []
-- nubBy' f (x:xs) = let res = nubBy' f xs
--                   in if any (f x) xs
--                      then x : res
--                      else  res



-- ñ)
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f x [] = []
deleteBy' f x (y:ys) = let res = deleteBy' f x ys
                       in if f x y
                          then res
                          else y : res

-- o)
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f [] = [[]]
groupBy' f [x] = [[x]]
groupBy' f (x:xs) = let (ys:yss) = groupBy' f xs
                   in if any (f x) ys
					             then ((x:ys):yss)
					             else ([x]:(ys:yss))

-- p)
concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f [] = []
concatMap' f (x:xs) = (++) (f x) (concatMap f xs)

-- q)
until' :: (a -> Bool) -> (a -> a) -> a -> a
until' p f x = if p (f x)
              then f x
              else until' p f (f x)
-- r )
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) = if p x
                      then x : takeWhile' p xs
                      else takeWhile' p []
-- s)
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) = if p x
                        then dropWhile' p xs
                        else x : xs

-- t)
span', break' :: (a -> Bool) -> [a] -> ([a],[a])

span' p [] = ([],[])
span' p (x:xs) = let (list1, list2) = span' p xs
                 in if p x
                    then (x : list1, list2)
                    else ([], x : xs)

break' p [] = ([],[])
break' p (x:xs) = let (list1, list2) = break' p xs
                 in if p x
                    then ([], x : xs)
                    else (x : list1, list2)


-- u)
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] [] = []
zipWith' f [] ys = []
zipWith' f xs [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- v )
zipApply :: [(a -> b)] -> [a] -> [b]
zipApply fs xs = zipWith' id fs xs

-- w )
enumerate [] n = []
enumerate (x:xs) n = (n, x) : enumerate xs (n + 1)

index :: [a] -> [(Int,a)]
index xs = enumerate xs 0

-- x )
applyN :: Int -> (a -> a) -> a -> a
applyN 0 f x = x
applyN 1 f x = f x
applyN n f x = applyN (n-1) f (f x)

-- y)
-- iterate f x == [f x, f (f x), f (f (f x)), ...]
iterate' :: (a -> a) -> a -> [a]
iterate' f x = f x : iterate' f (f x)

-- para probar: suma1 = (+) 1

-- z )
findIndex' :: (a -> Bool) -> [(Int,a)] -> Maybe' Int
findIndex' f [] = Nothing'
findIndex' f (x:xs) = if f (snd x)
                then Just' (fst x)
                else findIndex' f xs

findIndex :: (a -> Bool) -> [a] -> Maybe' Int
findIndex f xs = findIndex' f (index xs)

------------------------------------
-- 2. Currificacion
----1. Mejorar la definición de todas las funciones de la práctica 1, haciendo uso de funciones de
-- alto orden. Escribir las funciones estilo pointfree siempre que sea posible.
--
id':: a -> a
id' = id

const':: a -> b -> a
const' = (\ x y -> x)

fst':: (a,b) -> a
fst' = (\ (x, y) -> x)

snd':: (a,b) -> b
snd' = (\ (x, y) -> y)

swap'::(a,b) -> (b,a)
swap' = (\(x, y) -> (y, x))

head':: [a] -> a
head' = (\(x:xs) -> x)

tail':: [a] -> [a]
tail' = (\ (x:xs) -> xs)

-- sum', product' :: Num a => [a] -> a
--
-- sum' [] = 0
-- sum' (x:xs) = x + sum xs 
--
-- product' [] = 1
-- product' (x:xs) = x * product' xs

elem', notElem' :: Eq a => a -> [a] -> Bool

elem' = any' . (==)

notElem' = all' . (/=)

concat' :: [[a]] -> [a]
concat' = concatMap id

-- (!!!) :: [a] -> Int -> a
-- (!!!) [] y = error "No existe index"
-- (!!!) (x:xs) 0 = x
-- (!!!) (x:xs) y = (!!!) xs (y-1)
-- (!!!) (x:xs) y = (!!!) xs (y-1)

-- take' :: Int -> [a] -> [a]
-- take' 0 (y:ys) = []
-- take' x [] = []
-- take' x (y:ys) = y:(take' (x-1) ys)
--
-- drop' :: Int -> [a] -> [a]
-- drop' 0 ys = ys
-- drop' x [] = []
-- drop' x (y:ys) = (drop' (x-1) ys)

zip' :: [a] -> [b] -> [(a,b)]
zip' = zipWith' (\ x y -> (x, y) )

-- splitAt' :: Int -> [a] -> ([a], [a])
-- splitAt' 0 ys = ([], ys)
-- splitAt' x [] = ([], [])
-- splitAt' x (y:ys) =  let (list1, list2) = splitAt' (x-1) ys
--                          in (y:list1, list2)

-- maximum', minimum' :: Ord a => [a] -> a
--
-- maximum' [] = error "No hay elementos"
-- maximum' [x] = x
-- maximum' (x:xs) = max x (maximum' xs)
--
-- minimum' [] = error "No hay elementos"
-- minimum' [x] = x
-- minimum' (x:xs) = min x (minimum' xs)
--
-- lookup' :: Eq a => a -> [(a,b)] -> Maybe' b
-- lookup' x [] = Nothing'
-- lookup' x (y:ys) = if x == fst y
--                    then Just' (snd y)
--                    else lookup' x ys

unzip' :: [(a,b)] -> ([a],[b])
unzip' = (\ (xs) -> (map fst xs, map snd xs ))

-- tails' :: [a] -> [[a]]
-- tails' [] = [[]]
-- tails' (x:xs) = (x:xs):tails' xs

-- replicate' :: Int -> a -> [a]
-- replicate' 0 y = []
-- replicate' x y = y: replicate' (x-1) y

-- repeat' :: a -> [a]
-- repeat' x = x:repeat' x
--
-- cycle' :: [a] -> [a]
-- cycle' xs = xs ++ (cycle' xs)


nats' :: [Int]
nats' = [1..]
--
-- agrupar :: Eq a => [a] -> [[a]]
-- agrupar [] = [[]]
-- agrupar [x] = [[x]]
-- agrupar (x:xs) = let (ys:yss) = agrupar xs
--                  in if elem x ys
-- 					          then ((x:ys):yss)
-- 					          else ([x]:(ys:yss))
