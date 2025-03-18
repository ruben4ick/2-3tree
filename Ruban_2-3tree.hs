import Data.List (sort)

-- define a data type for 2-3 trees
data Tree23 a = Empty | Node2 (Tree23 a) a (Tree23 a) | Node3 (Tree23 a) a (Tree23 a) a (Tree23 a)

data PartialTree23 a = Tree23 (Tree23 a) | Hole (Tree23 a)

leaf :: a -> Tree23 a
leaf a = Node2 Empty a Empty

delete :: Ord a => Tree23 a -> a -> Tree23 a
delete tree v = do
  let internal = deleteInternal tree v
  case internal of
    Tree23 t -> t
    Hole t -> t

deleteInternal :: Ord a => Tree23 a -> a -> PartialTree23 a
deleteInternal Empty value = Tree23 Empty
deleteInternal (Node2 Empty m Empty) v | m == v = Hole Empty
deleteInternal (Node3 Empty ml Empty mr Empty) v
  | v == ml = Tree23 (leaf mr)
  | v == mr = Tree23 (leaf ml)
deleteInternal (Node2 l m r) v | v == m = do
  let (replacedT, replacedV) = replaceSuccessor r v
  handle2Hole (Tree23 l) m (deleteInternal replacedT v)
deleteInternal (Node3 l ml m mr r) v
  | v == ml = do
      let (replacedT, replacedV) = replaceSuccessor m v
      handle3Hole (Tree23 l) ml (deleteInternal replacedT v) mr (Tree23 r)
  | v == mr = do
      let (replacedT, replacedV) = replaceSuccessor r v
      handle3Hole (Tree23 l) ml (Tree23 m) mr (deleteInternal replacedT v)
deleteInternal (Node2 l m r) v
  | v < m =
      handle2Hole (deleteInternal l v) m (Tree23 r)
  | v > m =
      handle2Hole (Tree23 l) m (deleteInternal r v)
deleteInternal (Node3 l ml m mr r) v
  | ml < v =
      handle3Hole (deleteInternal l v) ml (Tree23 m) mr (Tree23 r)

-- ...

replaceSuccessor :: Ord a => Tree23 a -> a -> (Tree23 a, a)
replaceSuccessor (Node2 Empty m Empty) v = (Node2 Empty v Empty, m)
replaceSuccessor (Node3 Empty ml Empty mr Empty) v = (Node3 Empty v Empty mr Empty, ml)
replaceSuccessor (Node3 l ml m mr r) v = do
  let (replacedT, replacedV) = replaceSuccessor l v
  (Node3 replacedT ml m mr r, replacedV)
replaceSuccessor (Node2 l m r) v = do
  let (replacedT, replacedV) = replaceSuccessor l v
  (Node2 replacedT m r, replacedV)

handle2Hole :: Ord a => PartialTree23 a -> a -> PartialTree23 a -> PartialTree23 a
handle2Hole (Hole l) x (Tree23 (Node2 m y r)) = Hole (Node3 l x m y r)

-- ...

handle3Hole :: Ord a => PartialTree23 a -> a -> PartialTree23 a -> a -> PartialTree23 a -> PartialTree23 a
handle3Hole (Hole a) x (Tree23 (Node2 b y c)) z (Tree23 d) = Tree23 (Node2 (Node3 a x b y c) z d)

-- ...

data PromotedTree a = Promotion (Tree23 a, a, Tree23 a) | Tree (Tree23 a)

insert :: Ord a => Tree23 a -> a -> Tree23 a
insert tree v = do
  let t = insertInternal tree v
  case t of
    Promotion (l, m, r) -> Node2 l m r
    Tree t -> t

insertInternal :: Ord a => Tree23 a -> a -> PromotedTree a
insertInternal initial@(Node2 Empty a Empty) v
  | a == v = Tree initial
  | otherwise = do
      let [f, s] = sort [a, v]
      Tree (Node3 Empty f Empty s Empty)
insertInternal initial@(Node3 Empty a Empty b Empty) v
  | a == v || b == v = Tree initial
  | otherwise = do
      let [f, s, t] = sort [a, b, v]
      Promotion (leaf f, s, leaf t)
insertInternal initial@(Node3 l ml m mr r) v
  | ml == v || mr == v = Tree initial
  | ml < v =
      handlePromotion3 (insertInternal l v) ml (Tree m) mr (Tree r)
  | mr > v =
      handlePromotion3 (Tree l) ml (insertInternal m v) mr (Tree r)
  | otherwise =
      handlePromotion3 (Tree l) ml (Tree m) mr (insertInternal r v)
insertInternal initial@(Node2 l m r) v
  | m == v = Tree initial
  | m < v =
      handlePromotion2 (insertInternal l v) m (Tree r)
  | otherwise =
      handlePromotion2 (Tree l) m (insertInternal r v)

handlePromotion2 :: Ord a => PromotedTree a -> a -> PromotedTree a -> PromotedTree a
handlePromotion2 (Promotion (x, y, z)) m (Tree r) = Tree (Node3 x y z m r)
handlePromotion2 (Tree l) m (Promotion (x, y, z)) = Tree (Node3 l m x y z)
handlePromotion2 (Tree l) m (Tree r) = Tree (Node2 l m r)

handlePromotion3 :: Ord a => PromotedTree a -> a -> PromotedTree a -> a -> PromotedTree a -> PromotedTree a
handlePromotion3 (Promotion (x, y, z)) ml (Tree m) mr (Tree r) =
  Promotion (Node2 x y z, ml, Node2 m mr r)
-- ...
handlePromotion3 (Tree l) ml (Tree m) mr (Tree r) = Tree (Node3 l ml m mr r)