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
data Either' b a = Left' b | Right' a

mapEi:: (b -> c) -> Either' a b -> Either' a c
mapEi f (Left' x) = Left' x
mapEi f (Right' y) = Right' (f y)

-- 9.
data MTree a = L' (Maybe' a) | B'' a (MTree a) (MTree a)

mt = B'' "miku" (L' Nothing') (L' (Just' "rin"))

mapMT:: (a -> b) -> MTree a -> MTree b
mapMT f (L' x) = L' (mapMaybe f x)
mapMT f (B'' x mt1 mt2) = B'' (f x) (mapMT f mt1) (mapMT f mt2)

-- 10. (Desafío) data GenTree a = GNode a [GenTree a]

-- 1.2.
-- Con la definición de map dada para el tipo T del ejercicio anterior (llamémosla mapX), demostrar:
-- 1. mapX id = id

-- 2. mapX f . mapX g = mapX (f.g)
-- -- Llamada propiedad de fusión
-- 1.3.
-- Definir las siguientes funciones sobre los tipos algebraicos del punto 1, donde f es reemplazado
-- por cada uno de dichos tipos:
-- 1. find :: (a -> Bool) -> f a -> Maybe a

find' :: (a -> Bool) -> [a] -> Maybe' a
find' f [] = Nothing'
find' f (x:xs) = if f x
                then Just' x
                else find' f xs

findT :: (a -> Bool) -> Tree a -> Maybe' a
findT f EmptyT = Nothing'
findT f (NodeT x t1 t2) = if f x
                          then Just' x
                          else case (findT f t1) of
                            Just' x -> Just' x
                            Nothing' -> findT f t2

findNEL :: (a -> Bool) -> NonEmptyList a -> Maybe' a
findNEL f (Unit x) = if f x
                     then Just' x
                     else Nothing'
findNEL f (NECons x nel) = if f x
                           then Just' x
                           else findNEL f nel

findAL :: (a -> Bool) -> AppendList a -> Maybe' a
findAL f Nil = Nothing'
findAL f (Unitt x) = if f x
                     then Just' x
                     else Nothing'
findAL f (Append al1 al2) = case (findAL f al1) of
                                  Just' x -> Just' x
                                  Nothing' -> findAL f al2

findMaybe :: (a -> Bool) -> Maybe' a -> Maybe' a
findMaybe f Nothing' = Nothing'
findMaybe f (Just' x) = if f x
                        then Just' x
                        else Nothing'

ifThenElse:: (a -> Bool) -> a -> Maybe' a
ifThenElse f x = if f x
                 then Just' x
                 else Nothing'


findTt :: (a -> Bool) -> T a -> Maybe' a
findTt f (A x) = ifThenElse f x
findTt f (B tt) = findTt f tt
findTt f (C tt1 tt2) = case (findTt f tt1) of
                                  Just' x -> Just' x
                                  Nothing' -> findTt f tt2

findLT :: (a -> Bool) -> LTree a -> Maybe' a
findLT f (L xs) = find' f xs
findLT f (B' x lt1 lt2) = if f x
                          then Just' x
                          else case (findLT f lt1) of
                                  Just' x -> Just' x
                                  Nothing' -> findLT f lt2

--findEi :: (a -> Bool) -> Either' a b -> Maybe' a
findEi f (Left' x) = Nothing'
findEi f (Right' y) = ifThenElse f y

findMT :: (a -> Bool) -> MTree a -> Maybe' a
findMT f (L' x) = findMaybe f x
findMT f (B'' x mt1 mt2) = if f x
                         then Just' x
                         else case (findMT f mt1) of
                                 Just' x -> Just' x
                                 Nothing' -> findMT f mt2

-- 2. any, all :: (a -> Bool) -> f a -> Bool
any', all' :: (a -> Bool) -> [a] -> Bool

any' f [] = False
any' f (x:xs) = if f x
               then True
               else any' f xs

all' f [] = True
all' f (x:xs) = if f x
               then True && all' f xs
               else False

anyNEL, allNEL :: (a -> Bool) -> NonEmptyList a -> Bool

anyNEL f (Unit x) = if f x
		    then True
                    else False
anyNEL f (NECons x nel) = if f x
			  then True
                          else anyNEL f nel

anyNEL f (Unit x) = if f x
		    then True
                    else False
anyNEL f (NECons x nel) = if f x
			  then True && anyNEL f nel
                          else False

- ((>=4) . length) "jskajkasjd"
		

-- 3. partition :: (a -> Bool) -> f a -> ([a], [a])
