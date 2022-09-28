import Data.List
{-
    -- constante entiere, identifiant, declaration typee, definition

    c1 :: Int
    c1 = 1

    c2 :: Int
    c2 = 2+1

    c3 :: Int
    c3 = (+) 2 1 

    f4 :: Int -> Int
    f4 x = (+) 2 x

    mySub :: Int -> (Int -> Int)
    mySub    x       y      = x - y 


    -- application partielle (et eta reduction)

    myNeg :: Int -> Int
    myNeg x = mySub 0 x 

    myNeg' :: Int -> Int
    myNeg' = mySub 0 


    -- booleen et paresse

    b1 :: Bool
    b1 = True

    b2 :: Bool
    b2 = (b1 && False) || not b1

    b3 :: Bool
    b3 = 1>2 

    b4 :: Bool
    b4 = 1==2

    b5 :: Bool
    b5 = 1/=2

    -- liste d'entiers, nil, cons, liste en comprehension

    l1 :: [Int]
    l1 = []

    l2 :: [Int]
    l2 = 11:12:l1

    l3 :: [Int]
    l3 = undefined

    l4 :: [Int]
    l4 = undefined

    myNil :: [Int]
    myNil = []

    myCons :: Int -> [Int] -> [Int]
    myCons = (:)

    l5 :: [Int]
    l5 = [1..10]

    l6 :: [Int]
    l6 = [1,3..10]

    l7 :: [Int]
    l7 = [10,8..3]


    -- pattern matching

    myHead :: [Int] -> Int
    myHead (x:xs) = x

    myTail :: [Int] -> [Int]
    myTail (_:es) = es

    -- fonction recursive 

    --myAppend xs ys = (head xs) : (myAppend (tail xs) ys)

    myAppend :: [Int] -> [Int] -> [Int]
    myAppend (x:xs) ys = x : myAppend xs ys
    myAppend []     ys = ys




    myAppend' :: [Int] -> [Int] -> [Int]
    myAppend' xs ys | not (null xs) = head xs : myAppend' (tail xs) ys
                    | otherwise     = ys

    myAppend'' :: [Int] -> [Int] -> [Int]
    myAppend'' xs ys | null xs       = ys
                    | not (null xs) = head xs : myAppend'' (tail xs) ys

    myAppend4 :: [Int] -> [Int] -> [Int]
    myAppend4 (x:xs) ys = 
        let suite = myAppend4 xs ys 
        in x:suite
    myAppend4 []     ys = ys

    myAppend5 :: [Int] -> [Int] -> [Int]
    myAppend5 (x:xs) ys = x:suite where suite = myAppend5 xs ys
    myAppend5 []     ys = ys

    myAppend6 :: [Int] -> [Int] -> [Int]
    myAppend6 xs ys = myAppend6' xs 
        where myAppend6' :: [Int] -> [Int]
            myAppend6' (x:xs) = x:myAppend6' xs
            myAppend6' []     = ys
-}

-- a vous...

myInit :: [Int] -> [Int]
myInit (h:[]) = []
myInit (h:q) = h:myInit q

myLast :: [Int] -> Int
myLast (h:[]) = h
myLast (h:q) = myLast q


myNull :: [Int] -> Bool
myNull [] = True
myNull (_:_) = False

myNull' :: [Int] -> Bool
myNull' = undefined

myLength :: [Int] -> Int
myLength (h:q) = 1 + myLength q
myLength [] = 0


myReverse :: [Int] -> [Int]
myReverse l =
    let 
        myReverse'' :: [Int] -> [Int] -> [Int]
        myReverse'' (h:q) l2 = myReverse'' q (h:l2)
        myReverse'' [] l2 = l2
    in myReverse'' l []

--Complexitée aberrante
myReverse' :: [Int] -> [Int]
myReverse' [] = []
myReverse' l = myLast l : myReverse' (myInit l)


myConcat :: [[Int]] -> [Int]
myConcat l =
    let combine :: [[Int]] -> [Int] -> [Int]
        combine (h:q) l2 = mappend h (combine q l2)
        combine [] l2 = l2
    
    in combine l []

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (h:q) = h && myAnd q


myOr ::  [Bool] -> Bool
myOr [] = False
myOr (h:q) = h || myOr q

myProduct :: [Int] -> Int
myProduct [] = 1
myProduct (h:q) = h*(myProduct q)

-- pas d'element neutre pour max et min 

myTake :: Int -> [Int] -> [Int]
myTake 0 _ = []
myTake n (h:q) = (h:(myTake (n-1) q))

myDrop :: Int -> [Int] -> [Int]
myDrop _ [] = []
myDrop 0 (_:q) = q
myDrop 1 (_:q) = q
myDrop n (_:q) = myDrop (n-1) q

-- cette fonction existe sous le nom !!
myBangBang :: [Int] -> Int -> Int
myBangBang (h:_) 0 = h
myBangBang [] _ = error "index too large"
myBangBang (h:q) n = myBangBang q (n-1)

-- liste deja triee
myInsert :: Int -> [Int] -> [Int]
myInsert e (h:q) | e <= h = h:e:q
                 | e > h = h:(myInsert e q)
myInsert e [] = e:[]


myMaxList :: [Int] -> Int
myMaxList [] = error "Empty list"
myMaxList (h1:q1) =
    let aux :: [Int] -> Int -> Int
        aux (h:q) m | h >= m = aux q h
                    | h < m = aux q m
        aux [] m = m
    
    in aux q1 h1


myRemove :: Int -> [Int] -> [Int]
myRemove _ [] = []
myRemove x (h:q) | x == h = q
                 | x /= h = h:(myRemove x q)


mySort :: [Int] -> [Int]
mySort l =
    let aux :: [Int] -> [Int] -> [Int]
        aux [] l2 = l2
        aux l l2 = aux (myRemove m l) (m:l2) where m = (myMaxList l)
    in aux l []