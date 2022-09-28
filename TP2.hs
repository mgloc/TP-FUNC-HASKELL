-- TODO: definir recursivement

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile f (h:q)
    |f h = myDropWhile f q
    |otherwise = (h:q)
myDropWhile _ [] = []

myElem :: Eq a => a -> [a] -> Bool
myElem x (h:q) = x==h || myElem x q
myElem _ [] = False

myNotElem :: Eq a => a -> [a] -> Bool
myNotElem x l = not(myElem x l)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f (h:q)
    |f h = h:(myFilter f q)
    |otherwise = myFilter f q
myFilter _ [] = []

mySplitAt :: Int -> [a] -> ([a],[a])
mySplitAt 0 l = ([],l)
mySplitAt 1 (h:q) = ([h],q)
mySplitAt n (h:q) = (h:x,y) where (x,y) = (mySplitAt (n-1) q)
mySplitAt _ [] = ([],[])

myZip :: [a] -> [b] -> [(a,b)] 
myZip l1 l2 = case (l1,l2) of
    ([],_) -> []
    (_,[]) -> []
    (h1:q1,h2:q2) -> (h1,h2):(myZip q1 q2)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 
myZipWith f l1 l2 = case (l1,l2) of
    ([],_) -> []
    (_,[]) -> []
    (h1:q1,h2:q2) ->  (f h1 h2):(myZipWith f q1 q2)

myCurry :: ((a,b) -> c) -> a -> b -> c
myCurry f a b = f (a,b)

myUncurry :: (a -> b -> c) -> (a,b) -> c
myUncurry f (a,b) = f a b

myUnzip :: [(a,b)] -> ([a],[b])
myUnzip [] = ([],[])
myUnzip ((a,b):q) = ((a:x),(b:y)) where (x,y) = myUnzip q

-- define myZipWith' NON recursively
myZipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] 
myZipWith' = undefined

-- TODO: redefinir en utilisant foldr

myConcat' :: [[a]] -> [a]
myConcat' l = foldr (++) [] l

myMap' ::  (a -> b) -> [a] -> [b]
myMap' f = foldr (\x -> \fxs -> f x:fxs) [] 

myOr' ::  [Bool] -> Bool
myOr' = foldr (\x -> \fxs -> x || fxs) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x -> \fxs -> f x || fxs) False

myAll :: (a -> Bool) -> [a] -> Bool
myAll f = foldr (\x -> \fxs -> f x && fxs) True

myProduct :: [Int] -> Int
myProduct = foldr (\x -> \fxs -> x*fxs) 1

mySum :: [Int] -> Int
mySum = foldr (\x -> \fxs -> x + fxs) 0

mySort' :: [Int] -> [Int]
mySort' = foldr (\x -> \fxs -> myInsertCroissant x fxs) []
  where myInsertCroissant e (h:q)
            |e<h = (e:h:q)
            |e>=h = (h:myInsertCroissant e q)
        myInsertCroissant e [] = [e]


myReverse' :: [a] -> [a]
myReverse' = foldr (\x -> \fxs -> fxs ++ [x]) []

-- define recursively

myElem' :: Eq a => a -> [a] -> Bool
myElem' y (x:xs)
    |x==y =True
    |x/=y =myElem' y xs
myElem' y [] = False

myNotElem' :: Eq a => a -> [a] -> Bool
myNotElem' y = not.(myElem' y)

-- TODO: calculuer les 50 plus petits nombres premiers 2, 3, 5, 7, 11...

crible :: [Int] -> [Int]
crible (x:xs)
    |not(isDivisible x c) = x:c
    |otherwise = c
    where isDivisible x (h:q) = (mod x h == 0)||isDivisible x q
          isDivisible _ [] = False
          c = crible xs
crible [] = []

myCrible :: Int -> [Int]
myCrible n
    |n>1 = [n,n-1..2]
    |otherwise []

test2 = take 50 premiers