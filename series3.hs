import FPPrac.Trees
import Test.QuickCheck
import Data.List
import Data.Maybe

--Binary tree with Ints at the internal nodes and at the leaves.
data Tree1a = Leaf1a Int
            | Node1a Int Tree1a Tree1a
            deriving Show

--Binary Tree with (Int, Int) at the internal nodes and at the leaves
data Tree1b = Leaf1b (Int, Int)
            | Node1b (Int, Int) Tree1b Tree1b
            deriving Show

--Binary Tree that contains Ints at the leaves but no information at the nodes
data Tree1c = Leaf1c Int
            | Node1c Tree1c Tree1c
            deriving Show

data Tree1d = Leaf1d (Int, Int)
            | Node1d [Tree1d]
            deriving Show

class PP a where 
    pp:: a -> RoseTree

instance PP Tree1a where 
    pp = pp1a

instance PP Tree1b where 
    pp = pp1b

instance PP Tree1c where
    pp = pp1c

instance PP Tree1d where 
    pp = pp1d 

instance PP TreeInt where
    pp = ppInt

class MIRROR a where 
    binMirror:: a -> a

instance MIRROR Tree1a where
    binMirror = binMirror1a

pp1a:: Tree1a -> RoseTree
pp1a (Leaf1a x) = RoseNode (show x) []
pp1a (Node1a x t1 t2) = RoseNode (show x) [pp1a t1, pp1a t2]

tree1a::Tree1a
tree1a = (Node1a 4 (Leaf1a 5) (Node1a 8 (Node1a 6 (Leaf1a 7) (Leaf1a 9)) (Leaf1a 2)))

show1a = showRoseTree $ pp1a tree1a

pp1b:: Tree1b -> RoseTree
pp1b (Leaf1b x) = RoseNode (show x) []
pp1b (Node1b x t1 t2) = RoseNode (show x) [pp1b t1, pp1b t2]

tree1b::Tree1b
tree1b = (Node1b (2,2) (Leaf1b (2,3)) (Node1b (3,4) (Leaf1b (6,3)) (Leaf1b (1,4))))

show1b = showRoseTree $ pp1b tree1b

pp1c:: Tree1c -> RoseTree
pp1c (Leaf1c x) = RoseNode (show x) []
pp1c (Node1c t1 t2) = RoseNode "" [pp1c t1, pp1c t2]

tree1c::Tree1c
tree1c = (Node1c (Leaf1c (2)) (Node1c (Leaf1c (6)) (Leaf1c (1))))

show1c = showRoseTree $ pp1c tree1c

pp1d:: Tree1d -> RoseTree
pp1d (Leaf1d x) = RoseNode (show x) []
pp1d (Node1d ts) = RoseNode "" (map pp1d ts) 

tree1d::Tree1d
tree1d = (Node1d [(Leaf1d (2,3)), Node1d [(Leaf1d(3,4)), (Leaf1d(6,2)), (Leaf1d(1,5))]])

show1d = showRoseTree $ pp1d tree1d

--Exercise 20
treeAdd:: Int -> Tree1a -> Tree1a
treeAdd x (Leaf1a l) = (Leaf1a (l + x))
treeAdd x (Node1a n t1 t2) = (Node1a (n+x) (treeAdd x t1) (treeAdd x t2))

treeSquare:: Tree1a -> Tree1a
treeSquare (Leaf1a l) = (Leaf1a (l ^ 2))
treeSquare (Node1a n t1 t2) = (Node1a (n ^ 2) (treeSquare t1) (treeSquare t2))


treeAdd' n = mapTree (+n)
treeSquare' = mapTree (^2)

mapTree:: (Int -> Int) -> Tree1a -> Tree1a
mapTree f (Leaf1a l) = (Leaf1a (f l))
mapTree f (Node1a n t1 t2) = (Node1a (f n) (mapTree f t1) (mapTree f t2))

addNode:: Tree1b -> Tree1a
addNode (Leaf1b (a,b)) = (Leaf1a (a+b))
addNode (Node1b (a,b) t1 t2) = (Node1a (a+b) (addNode t1) (addNode t2))

mapTree1b:: ((Int, Int) -> Int) -> Tree1b -> Tree1a
mapTree1b f (Leaf1b (a,b)) = (Leaf1a (f (a,b)))
mapTree1b f (Node1b (a,b) t1 t2) = (Node1a (f (a,b)) (mapTree1b f t1) (mapTree1b f t2))

addition (a,b) = a+b
multiplication (a,b) = a*b

--exercise 21
binMirror1a :: Tree1a -> Tree1a
binMirror1a (Leaf1a x) = (Leaf1a x)
binMirror1a (Node1a x t1 t2) = (Node1a x (binMirror1a t2) (binMirror1a t1))

binMirror1d ::Tree1d -> Tree1d
binMirror1d (Leaf1d (a,b)) = (Leaf1d (b,a))
binMirror1d (Node1d ts) = (Node1d (map binMirror1d (reverse ts)))

--Exercise
data TreeInt = LeafInt
             | NodeInt Int TreeInt TreeInt
             deriving Show

ppInt:: TreeInt -> RoseTree
ppInt LeafInt = RoseNode "" []
ppInt (NodeInt i t1 t2) = RoseNode (show i) [ppInt t1, ppInt t2]

--Exercise 22
sortedTree :: TreeInt
sortedTree = (NodeInt 6 (NodeInt 3 (LeafInt) (LeafInt) ) (NodeInt 7 (LeafInt) (LeafInt)))


insertTree :: TreeInt -> Int -> TreeInt
insertTree (LeafInt) x  = (NodeInt x LeafInt LeafInt)
insertTree (NodeInt i t1 t2) x  | x <= i = (NodeInt i (insertTree t1 x) t2)
                                | otherwise = (NodeInt i (t1) (insertTree t2 x))

makeTree :: [Int] -> TreeInt
makeTree [] = LeafInt
makeTree (x:xs) = insertTree (makeTree xs) x

makeTree' :: [Int] -> TreeInt
makeTree' xs = foldl (insertTree) LeafInt xs

makeList :: TreeInt -> [Int]
makeList (LeafInt) = []
makeList (NodeInt i t1 t2) = makeList t1 ++ [i] ++ makeList t2

treeSort:: [Int] -> [Int]
treeSort xs = makeList $ makeTree xs
prop_treeSort xs = treeSort xs == sort xs

sortTree:: TreeInt -> TreeInt
sortTree t = makeTree $ makeList t

--Exercise 23
subtreeAt :: TreeInt -> Int -> Maybe TreeInt
subtreeAt (LeafInt) x = Nothing
subtreeAt (NodeInt i t1 t2) x | x == i = Just (NodeInt i t1 t2)
                              | x > i = subtreeAt t2 x
                              | otherwise = subtreeAt t1 x

--Exercise 24
cutOffAt :: Tree1a -> Int -> Tree1a
cutOffAt (Leaf1a x) _ = (Leaf1a x)
cutOffAt (Node1a x t1 t2) 0 = (Leaf1a x)
cutOffAt (Node1a x t1 t2) n = (Node1a x (cutOffAt t1 (n-1)) (cutOffAt t2 (n-1))) 

--Exercise 25
data BinTree a = Leaf
               | Node a (BinTree a) (BinTree a)
               deriving Show

myBinTree:: a -> BinTree a
myBinTree a = Node a (Node a Leaf Leaf) (Leaf)

--Question: How to make this an instance? 
--I have trouble with Kinds (* -> * and *)
--Another issue: No instance for (Show a) found
instance (Show a) => PP (BinTree a) where
     pp = ppBinTree

ppBinTree:: (Show a) => BinTree a -> RoseTree
ppBinTree (Leaf) = RoseNode "" []
ppBinTree (Node n t1 t2) = RoseNode (show n) [ppBinTree t1, ppBinTree t2]


instance (Show a) => MIRROR (BinTree a) where
    binMirror = binMirrorBin

binMirrorBin:: BinTree a -> BinTree a
binMirrorBin (Leaf) = Leaf
binMirrorBin (Node n t1 t2) = Node n (binMirrorBin t2) (binMirrorBin t1)

instance Functor BinTree where
    fmap f Leaf = Leaf
    fmap f (Node n t1 t2) = Node (f n) (fmap f t1) (fmap f t2)



--Exercise 26
data MyList a = Nil 
              | Cons a (MyList a)
              deriving (Show, Eq)
mylst = Cons 1 $ Cons 2 $ Cons 3 $ Nil

fromList:: [a] -> MyList a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

instance Functor MyList where
    fmap f Nil = Nil
    fmap f (Cons x xs) = (f x) `Cons` fmap f xs


prop_fmap xs = fmap id (fromList xs) == fromList xs


--How do I use quickcheck in this way? 
-- I want to test that (*1) results in the same mylst or (+0)




