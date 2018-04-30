import FPPrac.Trees

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

-- class PP a where 
-- 	pp:: a -> RoseTree

pp1a:: Tree1a -> RoseTree
pp1a (Leaf1a x) = RoseNode (show x) []
pp1a (Node1a x t1 t2) = RoseNode (show x) [pp1a t1, pp1a t2]

tree1a::Tree1a
tree1a = (Node1a 4 (Leaf1a 5) (Node1a 8 (Leaf1a 3) (Leaf1a 2)))

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
