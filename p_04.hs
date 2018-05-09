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
-- 2. 
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

pal = (NodeT "miku" (NodeT "rin" (NodeT "len" EmptyT EmptyT) (NodeT "megpoid" EmptyT EmptyT)) (NodeT "luka" EmptyT EmptyT))

mapT:: (a -> b) -> Tree a -> Tree b
mapT f EmptyT = EmptyT
mapT f (NodeT x t1 t2) = NodeT (f x) (mapT f t1) (mapT f t2)

-- 3. 
data NonEmptyList a = Unit a | NECons a (NonEmptyList a)

necons = NECons "miku" (NECons "luka" (Unit "rin"))

mapNEL:: (a->b) -> NonEmptyList a -> NonEmptyList b
mapNEL f (Unit x) = Unit (f x)
mapNEL f (NECons x nel) = NECons (f x) (mapNEL f nel)

-- 4. 
data AppendList a = Nil | Unitt a | Append (AppendList a) (AppendList a)

al = Append (Append (Unitt "miku") (Unitt "rin")) Nil

mapAL:: (a -> b) -> AppendList a -> AppendList b
mapAL f Nil = Nil
mapAL f (Unitt x) = Unitt (f x)
mapAL f (Append al1 al2) = Append (mapAL f al1) (mapAL f al2)

-- 5. 
data Maybe' a = Nothing' | Just' a

mapMaybe:: (a->b) -> Maybe' a -> Maybe' b
mapMaybe f Nothing' = Nothing'
mapMaybe f (Just' x) = Just' (f x)

-- 6. 
data T a = A a | B (T a) | C (T a) (T a)

tt = C (A "rin") (B (A "miku"))

mapTt:: (a -> b) -> (T a) -> (T b)
mapTt f (A x) = A (f x)
mapTt f (B t) = B (mapTt f t)
mapTt f (C t1 t2) = C (mapTt f t1) (mapTt f t2)

-- 7. 
data LTree a = L [a] | B' a (LTree a) (LTree a)

lt = B' "miku" (L ["rin", "len"]) (L [])

mapLT:: (a -> b) -> LTree a -> LTree b
mapLT f (L xs) = L (map f xs)
mapLT f (B' x lt1 lt2) = B' (f x) (mapLT f lt1) (mapLT f lt2)

-- 8. 
data Either b a = Left b | Right a

mapEi:: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapEi f g (Left x) = Left (f x)
mapEi f g (Right y) = Right (g y)

-- 9. 
data MTree a = L (Maybe a) | B a (MTree a) (MTree a)

--mapMT:: (a -> b) -> MTree a -> MTree b
--mapMT (L x) = 

-- 10. (Desafío) data GenTree a = GNode a [GenTree a]
