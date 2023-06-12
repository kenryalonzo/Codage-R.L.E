-- Okay les amis on commence fort avec ce Td, vous me suivez j'espere

data QuadTree a = Leaf a | Node (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)
    deriving(Show)

-- Okay les amis on suit rapidement avec la fonction flattern

flattern :: (QuadTree a) -> [a]
flattern (Leaf x) = [ x ]
flattern (Node x y z t) = flattern (x) ++ flattern (y) ++ flattern (z) ++ flattern (t)

-- On part tout de suite sur les fonctions size et heigth

size :: (QuadTree a) -> Int 
size (Leaf x) = 0
size (Node x y z t) = 1 + size (x) + size (y) + size (t)

-- Allons maintenant nous attacquer a la fonctions height si vous voyez ce que je veux dire

{-height :: (QuadTree a) -> Int
height (Leaf x) = 0
height (Node x y z t) = 1 + max (max (height x) (height y) max (height z) (height t))
-}

-- ssyons d'ecrire la fonction map associer a ce QuadTree

mapx :: (a -> b) -> [a] -> [b]
mapx f [] = [ ]
mapx f (x : xs) = f(x) : (mapx f xs)

-- gerons maintenant QuadTree

mapQ :: (a -> b) ->(QuadTree a) -> (QuadTree b)
mapQ f (Leaf x) = Leaf (f x)
mapQ f (Node x y z t) = Node (mapQ f x) (mapQ f y) (mapQ f z) (mapQ f t)
