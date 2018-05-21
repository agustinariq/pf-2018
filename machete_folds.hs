foldr:: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f [] = error "La lista es vacia"
foldr1' f [x] = x
foldr1' f (x:xs) = f x (foldr1' f xs)
-- 2.
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x:xs) = f x xs (recr f z xs)
