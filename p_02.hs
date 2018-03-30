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
                    else (list1, xs)

break' p [] = ([],[])

-- u) zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- v ) zipApply :: [(a -> b)] -> [a] -> [b]
