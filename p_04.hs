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

anyT, allT :: (a -> Bool) -> Tree a -> Bool

anyT f EmptyT = False
anyT f (NodeT x t1 t2) = if f x
                          then True
                          else case (anyT f t1) of
                            True -> True
                            False -> anyT f t2

allT f EmptyT = True
allT f (NodeT x t1 t2) = if f x
                          then True && allT f t1 && allT f t2
                          else False


anyNEL, allNEL :: (a -> Bool) -> NonEmptyList a -> Bool

anyNEL f (Unit x) = f x
anyNEL f (NECons x nel) = if f x
                          then True
                          else (anyNEL f nel)

allNEL f (Unit x) = f x
allNEL f (NECons x nel) = if f x
                          then True && allNEL f nel
                          else False

anyAL, allAL :: (a -> Bool) -> AppendList a -> Bool


anyAL f Nil = False
anyAL f (Unitt x) = f x
anyAL f (Append al1 al2) = anyAL f al1 || anyAL f al2

allAL f Nil = True
allAL f (Unitt x) = f x
allAL f (Append al1 al2) = allAL f al1 && allAL f al2

--anyMaybe, allMaybe :: (a -> Bool) -> Maybe a -> Bool

anyMaybe f Nothing' = False
anyMaybe f (Just' x) = f x

allMaybe f Nothing' = False
allMaybe f (Just' x) = f x

anyTt, allTt :: (a -> Bool) -> T a -> Bool

anyTt f (A x) = f x
anyTt f (B t) = anyTt f t
anyTt f (C t1 t2) = anyTt f t1 || anyTt f t2

allTt f (A x) = f x
allTt f (B t) = allTt f t
allTt f (C t1 t2) = allTt f t1 && allTt f t2

anyLT, allLT :: (a -> Bool) -> LTree a -> Bool

anyLT f (L xs) = any' f xs
anyLT f (B' x lt1 lt2) = f x || anyLT f lt1 || anyLT f lt2

allLT f (L xs) = any' f xs
allLT f (B' x lt1 lt2) = f x && allLT f lt1 && allLT f lt2

anyEi, allEi :: (a -> Bool) -> Either' b a -> Bool

anyEi f (Left' x) = False
anyEi f (Right' y) = f y

allEi f (Left' x) = False
allEi f (Right' y) = f y

anyMT, allMT :: (a -> Bool) -> MTree a -> Bool

anyMT f (L' m) = anyMaybe f m
anyMT f (B'' x mt1 mt2) = f x || anyMT f mt1 || anyMT f mt2

allMT f (L' m) = allMaybe f m
allMT f (B'' x mt1 mt2) = f x && allMT f mt1 && anyMT f mt2

-- ((>=4) . length) "jskajkasjd"

-- 3. partition :: (a -> Bool) -> f a -> ([a], [a])
-- Entrega 5
-- 2.2.
-- Definir para listas:
-- 1.
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f [] = error "La lista es vacia"
foldr1' f [x] = x
foldr1' f (x:xs) = f x (foldr1' f xs)
-- 2.
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

-- 2.3.
-- Definir todas las funciones ya definidas sobre listas foldr, foldr1 o recr (dependiendo del
-- caso), y sobre árboles binarios con foldT. Tener en cuenta que en algunos casos pueden ser difíciles,
-- o que no se puedan realizar con recursión estructural.

head' :: [a] -> a
head' = foldr1' (\ x r -> x)

tail' :: [a] -> [a]
tail' = recr (\ x xs r -> xs) (error "La lista es vacia")

--
null' :: [a] -> Bool
null' = foldr (\ x r -> False) True

sum', product' :: Num a => [a] -> a

sum' = foldr (\ x r -> x + r) 0

product' = foldr (\ x r -> x * r) 1

length' :: [a] -> Int
length' = foldr (\ x r -> 1 + r) 0

elem' :: Eq a => a -> [a] -> Bool
elem' y xs = foldr (\ x r -> x == y || r ) False xs

and', or' :: [Bool] -> Bool

and' = foldr (\ x r -> x && r) True

or' = foldr (\ x r -> x || r) False

-- last' :: [a] -> a
-- last' [] = error "La lista es vacia"
-- last' [x] = x
-- last' (x:xs) = last' xs

last' :: [a] -> a
last' = foldr1' (\ x r -> r)


-- init' :: [a] -> [a]
-- init' [] = error "La lista es vacia"
-- init' [x] = []
-- init' (x:xs) = x:(init' xs)

--init' :: [a] -> [a]
--init' xs =  recr (\ x xs r -> [1]) (\ x -> if null x then error "salala" else head x ) xs

-- snoc no es recursivo
--snocc e [] = [e]
--snocc e xs = xs ++ [e]

snoc' :: [a] -> a -> [a]
snoc' xs e = recr (\ x xs r -> (x:xs) ++ [e] ) [e] xs

--reverse'' [] = []
--reverse'' (x:xs) = reverse'' xs ++ [x]

reverse' :: [a] -> [a]
reverse' = foldr (\ x r -> r ++ [x]) []

-- subset' :: Eq a => [a] -> [a] -> Bool
-- subset' [] ys = True
-- subset' (x:xs) ys = (elem' x ys) && subset' xs ys

--subset' :: Eq a => [a] -> [a] -> Bool
--subset' (x:xs) ys = foldr (belongsToSet) True xs ys where belongsToSet x r ys = (elem x ys) && r

-- elem' :: Eq a => a -> [a] -> Bool
-- elem' y xs = foldr (\ x r -> x == y || r ) False xs


subset' :: Eq a => [a] -> [a] -> Bool
subset' xs ys = foldr (\ x r -> elem x ys && r) True xs

(+++) :: [a] -> [a] -> [a]
(+++) xs ys = foldr (\ x r -> x:r) ys xs

concat' :: [[a]] -> [a]
concat' = foldr (\ x r -> x ++ r) []

--(!!!) :: [a] -> Int -> a
--(!!!) xs i = recr (\ x xs r ->)

-- take :: Int -> [a] -> [a]
-- drop :: Int -> [a] -> [a]
-- zip :: [a] -> [b] -> [(a,b)]
-- splitAt :: Int -> [a] -> ([a], [a])
-- maximum, minimum :: Ord a => [a] -> a
-- unzip :: [(a,b)] -> ([a],[b])
-- tails :: [a] -> [[a]]
-- nub :: (Eq a) => [a] -> [a]
-- isPrefixOf :: Eq a => [a] -> [a] -> Bool
-- elemIndex :: Eq a => a -> [a] -> Maybe Int
-- group :: Eq a => [a] -> [[a]]
-- delete :: Eq a => a -> [a] -> [a]
-- map :: (a -> b) -> [a] -> [b]
-- filter :: (a -> Bool) -> [a] -> [a]
-- any, all :: (a -> Bool) -> [a] -> Bool
-- find :: (a -> Bool) -> [a] -> Maybe a
-- countBy :: (a -> Bool) -> [a] -> Int
-- partition :: (a -> Bool) -> [a] -> ([a], [a])
-- concatMap :: (a -> [b]) -> [a] -> [b]
-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
