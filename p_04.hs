-- entrega 04
-- 1.
-- Map
-- 1.1.
-- Definir la función map para los siguientes tipos algebraicos:
-- 1.
--data [a] = [] | a : [a]

map':: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs
-- 2. data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
-- 3. data NonEmptyList a = Unit a | NECons a (NonEmptyList a)
-- 4. data AppendList a = Nil | Unit a | Append (AppendList a) (AppendList a)
-- 5. data Maybe a = Nothing | Just a
-- 6. data T a = A a | B (T a) | C (T a) (T a)
-- 7. data LTree a = L [a] | B a (M a) (M a)
-- 8. data Either b a = Left b | Right a
-- 9. data MTree a = L (Maybe a) | B a (MTree a) (MTree a)
-- 10. (Desafío) data GenTree a = GNode a [GenTree a]
