
-- questions ? 













-- purely functional data structures

-- what is a purely functional data structure? when you "modify" a
-- data structure, you get a modified copy of the data structure. the
-- original one has not changed. this is the only way to do in
-- haskell. remember the type list []. remember the list of random
-- integer in quickcheck (the new version is returned). in general it
-- can make programming simpler. because of local reasonning, where
-- side effects requires global reasonning (who has accessed the data
-- structure before me, between two of my accesses?)

-- in imperative languages, purely functionnal data stucture are also
-- used. in particular when it makes programming simpler. programming
-- is quite complex in concurrent programs...

-- list revisited

-- list promotes left access / construction

data List a = Nil | Cons a (List a) deriving (Show,Eq) -- the recursion is in the second arg of Cons

-- head/tail are O(1)

myHead :: List a -> a
myHead (Cons x xs) = x

myTail :: List a -> List a
myTail (Cons x xs) = xs

-- init/last are O(n) where n = length xs

myLast :: List a -> a
myLast (Cons x Nil) = x
myLast (Cons x xs)  = myLast xs

myInit :: List a -> List a
myInit (Cons x Nil) = Nil
myInit (Cons x xs)  = Cons x (myInit xs)

myAppend :: List a -> List a -> List a
myAppend (Cons x xs) ys = Cons x (myAppend xs ys)
myAppend Nil         ys = ys

-- we can promote right access when an algorithm is more right
-- oriented that left oriented.

-- Tsil promotes right access / construction

data Tsil a = Lin | Snoc (Tsil a) a deriving (Show,Eq) -- the recursion is in the first param of Snoc

-- tsal/tini are O(1)

myTsal :: Tsil a -> a
myTsal (Snoc xs x) = x

myTini :: Tsil a -> Tsil a
myTini (Snoc xs x) = xs

-- but deah/liat are O(n)

myDeah :: Tsil a -> a
myDeah (Snoc Lin x) = x
myDeah (Snoc xs  x) = myDeah xs

myLiat :: Tsil a -> Tsil a
myLiat (Snoc Lin x) = Lin
myLiat (Snoc xs  x) = Snoc (myLiat xs) x

myDneppa :: Tsil a -> Tsil a -> Tsil a
myDneppa xs Lin = xs
myDneppa xs (Snoc ys y) = Snoc (myDneppa xs ys) y

-- purely functional queue

type Queue1 a = [a]

isEmpty1 :: Queue1 a -> Bool
isEmpty1 = null -- O(1)

enQueue1 :: a -> Queue1 a -> Queue1 a 
enQueue1 = (:) -- O(1)

deQueue1 :: Queue1 a -> (a,Queue1 a)
deQueue1 xs = (last xs,init xs) -- O(n) where n = length xs

-- if we evaluate the complexity of a sequence: n * enqueue ; n * dequeue
-- queue1 : n*O(1) + O(n) + O(n-1) + ++ O(1) = O(n^2)

-- list zipper for editable list

type ListZ a = ([a],[a])

mkZipL :: [a] -> ListZ a
mkZipL xs = ([],xs)

unzipL :: ListZ a -> [a]
unzipL (ls,rs) = reverse ls ++ rs

goNext :: ListZ a -> ListZ a  -- O(1)
goNext (ls,cursor:rs) = (cursor:ls,rs)

goPrevious :: ListZ a -> ListZ a -- O(1)
goPrevious (cursor:ls,rs) = (ls,cursor:rs)

transfoElt :: (a -> a) -> ListZ a -> ListZ a  -- O(1)
transfoElt f (cursor:ls,rs) = (f cursor:ls,rs)

-- n-ary tree depth first 

data Rose a = Rose a (Forest a) deriving (Show, Eq)

type Forest a = [Rose a]

label :: Rose a -> a
label (Rose a ts) = a

subTree :: Rose a -> Forest a
subTree (Rose a ts) = ts

dfs :: Rose a -> Forest a
dfs t = t:concat (map dfs (subTree t))


-- TODO 1: a third representation for list, Seq promotes concatenation

data Seq a = None | One a | Join (Seq a) (Seq a) deriving (Show,Eq) -- the recursion is in both params of Join

-- maintain a normal form: there is no None in a non empty Seq

myHeadS :: Seq a -> a
myHeadS (Join l1 l2) = myHeadS l1
myHeadS (One a) = a

myTailS :: Seq a -> Seq a
myTailS (Join (One _) l2) = l2
myTailS (Join l1 l2) = Join (myTailS l1) l2
myTailS a = a

myInitS :: Seq a -> Seq a
myInitS (Join l1 (One _)) = l1
myInitS (Join l1 l2) = Join l1 (myInitS l2)
myInitS a = a

myLastS :: Seq a -> a
myLastS (Join l1 l2) = myLastS l2
myLastS (One a) = a

myAppendS :: Seq a -> Seq a -> Seq a
myAppendS = Join

myReverseS :: Seq a -> Seq a
myReverseS (Join l1 l2) = Join (myReverseS l2) (myReverseS l1)
myReverseS a = a



-- Function only used to test
myListToSeq :: [a] -> Seq a
myListToSeq [h1] = One h1
myListToSeq (h:q) = Join (One h) (myListToSeq q)
myListToSeq [] = None

mySeqToList :: Seq a -> [a]
mySeqToList (One h1) = [h1]
mySeqToList (Join s1 s2) = (mySeqToList s1) ++ (mySeqToList s2)
mySeqToList None = []


-- Listes à utiliser pour les tests
li1 = [1,2,3,4]
li2 = [4,3,4,1]
listes = [[1,2,3],[1,5,6,6],[2]]


listToSeqApply :: (Seq a -> Seq a) -> [a] -> [a]
listToSeqApply f l = mySeqToList (f (myListToSeq l))

test_myHeadS :: Eq a => [a] -> Bool
test_myHeadS l = myHeadS (myListToSeq l) == head l

test_myTailS :: Eq a => [a] -> Bool
test_myTailS l = (listToSeqApply myTailS l) ==  tail l

test_myInitS :: Eq a => [a] -> Bool
test_myInitS l = (listToSeqApply myInitS l) == init l

test_myLastS :: Eq a => [a] -> Bool
test_myLastS l = myLastS (myListToSeq l) == last l

test_myAppendS :: Eq a => [a] -> [a] -> Bool
test_myAppendS l1 l2 = mySeqToList(myAppendS (myListToSeq l1) (myListToSeq l2)) == (l1 ++ l2)

test_myReverseS :: Eq a => [a] -> Bool
test_myReverseS l = (listToSeqApply myReverseS l) == reverse l

-- TODO 2: purely functional queue

type Queue2 a = ([a],[a])

isEmpty2 :: Queue2 a -> Bool -- O(1)
isEmpty2 ([],[]) = True
isEmpty2 (xs,ys) = False

enQueue2 :: a -> Queue2 a -> Queue2 a -- O(1)
enQueue2 x (xs,ys) = (x:xs,ys)

deQueue2 :: Queue2 a -> (a,Queue2 a) -- O(1) or O(n) where n = length xs
deQueue2 (xs,h:q) = (h,(xs,q))
deQueue2 (xs,[]) = (last xs,([],reverse (init xs)))

-- if we evaluate the complexity of a sequence: n * enqueue ; n * dequeue
-- queue2 = O(n) n*(O(1)) +  O(n) + n*(O(1))

test_Q2_1 = enQueue2 3 ([1,2],[]) == ([3,1,2],[])

test_Q2_2 = deQueue2 ([1,2,3,4],[]) == (4,([],[3,2,1]))

test_Q2_3 = deQueue2 ([1,2],[3,4]) == (3,([1,2],[4]))


-- TODO 3: tree zipper for editable tree (zipper can be defined for any structure)

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving (Show,Eq)

data Ctx a = NodeL (Ctx a) (Tree a) | NodeR (Tree a) (Ctx a) | Here deriving (Show,Eq)

type TreeZ a = (Tree a,Ctx a)

mkZipT :: Tree a -> TreeZ a
mkZipT t = (t,Here)

goLeft :: TreeZ a -> TreeZ a -- O(1)
goLeft (Node t1 t2,c) = (t1,NodeL c t2)

goRight :: TreeZ a -> TreeZ a -- O(1)
goRight (Node t1 t2,c) = (t2,NodeR t2 c)

goUp :: TreeZ a -> TreeZ a -- O(1)
goUp (tL, NodeL c tR) = (Node tL tR,c)
goUp (tR, NodeR tL c) = (Node tL tR,c)

transfoTree :: (Tree a -> Tree a) -> TreeZ a -> TreeZ a -- O(1)
transfoTree f (t,c) = (f t,c)


-- TODO 4: a tree breadth first 

bfs :: Forest a -> Forest a
bfs [Rose a l] = (Rose a l):(bfs l)
bfs ((Rose a l):q) = (Rose a l):(bfs q)++(bfs l)
bfs [] = []

arbre :: Rose Int
arbre = (Rose 1 [Rose 2 [Rose 23 []],Rose 3 [], Rose 4 [Rose 5 []]])


-- TODO 5: a Map from key to value
-- where keys are sorted in Fork l (k,a) r: (all keys of l <= k) && (k < all keys of r)

data Map k a = Tip | Fork (Map k a) (k,a) (Map k a)
  deriving (Show,Eq)

emptyMap :: Map k a
emptyMap = Tip

lookupMap :: Ord k => k -> Map k a -> a -- O(?) where n = size map (but ? if balanced trees)
--Pour approximer au plus proche et non exact
lookupMap k (Fork Tip (k',a) d) | k < k' = a
lookupMap k (Fork g (k',a) Tip) | k > k' = a

--Fonction lookupMap
lookupMap k (Fork g (k',a) d) | k == k' = a
                              | k > k' = lookupMap k d
                              | k < k' = lookupMap k g



insertMap :: Ord k => (k,a) -> Map k a -> Map k a -- update si k deja present
insertMap (k,a) (Fork g (k',b) d) | k == k' = Fork g (k,a) d
                                  | k > k'  = Fork g (k',b) (insertMap (k,a) d)
                                  | k < k' = Fork (insertMap (k,a) g) (k',b) d
insertMap (k,a) Tip = Fork Tip (k,a) Tip

test_lookupMap :: Bool
test_lookupMap = lookupMap 2 (Fork (Fork (Fork Tip (2,2) Tip) (3,3) Tip) (4,4) (Fork Tip (5,5) Tip)) == 2

test_insertMap :: Bool
test_insertMap = insertMap (10,10) (Fork (Fork (Fork Tip (2,2) Tip) (3,3) Tip) (4,4) (Fork Tip (5,5) Tip)) == (Fork (Fork (Fork Tip (2,2) Tip) (3,3) Tip) (4,4) (Fork Tip (5,5) (Fork Tip (10,10) Tip)))

-- TODO 6: study the code and the complexity of the following data structure
-- both a list and an array (purely functional random-access list, kris okasaki, 1995)

-- we assume trees are complete

data CTree a = CNode (CTree a) a (CTree a) | CLeaf a deriving (Show,Eq)

-- tree random access

lookupCTree :: Int -> CTree a -> Int -> a -- O(log_2(n)) where n == nb_Nodes 
lookupCTree size (CLeaf x)     0 = x
lookupCTree size (CNode l x r) 0 = x
lookupCTree size (CNode l x r) i | i<=div size 2 = lookupCTree (div size 2) l (i-1)
                                 | otherwise     = lookupCTree (div size 2) r (i-1-div size 2)

updateCTree :: Int -> CTree a -> Int -> a -> CTree a -- O(log_2(n))
updateCTree size (CLeaf x)     0 y = CLeaf y
updateCTree size (CNode l x r) 0 y = CNode l y r
updateCTree size (CNode l x r) i y | i<=div size 2 = CNode (updateCTree (div size 2) l (i-1) y) x r 
                                   | otherwise     = CNode l x (updateCTree (div size 2) r (i-1-div size 2) y) 

-- both a list and an array

type SizedTree a = (Int,CTree a)

type ListArray a = [SizedTree a]

lookupFA :: ListArray a -> Int -> a -- O(nlog_2(n)) where n is the size/number of elements of the array
lookupFA ((s,t):sts) i | i<s       = lookupCTree s t i
                       | otherwise = lookupFA sts (i-s)

updateFA :: ListArray a -> Int -> a -> ListArray a -- O(nlog(n)) where n is the size/number of elements of the array
updateFA ((s,t):sts) i y | i<s       = (s,updateCTree s t i y):sts
                         | otherwise = (s,t):updateFA sts (i-s) y

emptyFA :: ListArray a
emptyFA = [] -- O(1)

isEmptyFA :: ListArray a -> Bool
isEmptyFA = null -- O(1)

consFA :: a -> ListArray a -> ListArray a -- O(1)
consFA x ((s1,t1):(s2,t2):ts) | s1==s2 = (1+s1+s2,CNode t1 x t2):ts
consFA x ts                            = (1,CLeaf x):ts

headFA :: ListArray a -> a -- O(1)
headFA ((1,CLeaf x)    :ts) = x
headFA ((s,CNode l x r):ts) = x

tailFA :: ListArray a -> ListArray a -- O(1)
tailFA ((1,CLeaf x)    :ts) = ts
tailFA ((s,CNode l x r):ts) = (div s 2,l):(div s 2,r):ts