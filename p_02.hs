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
-- j ) maybe :: b -> (a -> b) -> Maybe a -> b
-- k ) either :: (a -> c) -> (b -> c) -> Either a b -> c
-- l ) find :: (a -> Bool) -> [a] -> Maybe a
-- m) partition :: (a -> Bool) -> [a] -> ([a], [a])
-- n) nubBy :: (a -> a -> Bool) -> [a] -> [a]
-- ñ) deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
-- o) groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
-- p) concatMap :: (a -> [b]) -> [a] -> [b]
-- q) until :: (a -> Bool) -> (a -> a) -> a -> a
-- r ) takeWhile :: (a -> Bool) -> [a] -> [a]
-- s) dropWhile :: (a -> Bool) -> [a] -> [a]
-- t) span, break :: (a -> Bool) -> [a] -> ([a],[a])
-- u) zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- v ) zipApply :: [(a -> b)] -> [a] -> [b]
