module HW1.T3
  ( Tree(..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

data Tree a
  = Leaf
  | Branch Int (Tree a) a (Tree a)
  deriving Show

-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize Leaf                = 0
tsize (Branch size _ _ _) = size

-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth Leaf             = 1
tdepth (Branch _ l _ r) = max (tdepth l) (tdepth r) + 1

-- | Check if the element is in the tree, O(log n)
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf                 = False
tmember b (Branch _ l value r) = (b == value) || tmember b l || tmember b r

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert b Leaf                         = mkBranch Leaf b Leaf
tinsert b tree@(Branch _ l value r)
  | b == value = tree
  | b < value  = mkBranch (tinsert b l) value r
  | b > value  = mkBranch l value (tinsert b r)
  | otherwise  = error "incorrect value"

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch l val r
  | (tdepth l - tdepth r) < 2 && (tdepth l - tdepth r) > -2 = Branch (tsize l + 1 + tsize r) l val r
  | (tdepth l - tdepth r) == 2                              = turnLeft val l r
  | (tdepth l - tdepth r) == -2                             = turnRight val l r
  | otherwise                                               = error "incorrect tree"

getLeft :: Tree a -> Tree a
getLeft Leaf             = Leaf
getLeft (Branch _ l _ _) = l

getRight :: Tree a -> Tree a
getRight Leaf             = Leaf
getRight (Branch _ _ _ r) = r

getValue :: Tree a -> a
getValue Leaf                 = error "Leaf have not value" 
getValue (Branch _ _ value _) = value

turnLeft :: a -> Tree a -> Tree a -> Tree a
turnLeft val l r = 
  let left = getLeft l
      right = getRight l 
  in
    if tdepth left > tdepth right then ll val (getValue l) left right r
                                  else lr val (getValue l) (getValue right) left (getLeft right) (getRight right) r

ll :: a -> a -> Tree a -> Tree a -> Tree a -> Tree a
ll x y a b c = mkBranch a y (mkBranch b x c)

lr :: a -> a -> a -> Tree a -> Tree a -> Tree a -> Tree a -> Tree a
lr x y z a b c d = mkBranch (mkBranch a y b) z (mkBranch c x d)

turnRight :: a -> Tree a -> Tree a -> Tree a
turnRight val l r = 
  let left = getLeft r
      right = getRight r 
  in
    if tdepth right > tdepth left then rl val (getValue r) right left l
                                  else rr val (getValue r) (getValue left) right (getRight left) (getLeft left) l

rl :: a -> a -> Tree a -> Tree a -> Tree a -> Tree a
rl x y a b c = mkBranch (mkBranch c x b) y a

rr :: a -> a -> a -> Tree a -> Tree a -> Tree a -> Tree a -> Tree a
rr x y z a b c d = mkBranch (mkBranch d x c) z (mkBranch b y a)

-- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a
tFromList l = case l of
  []   -> Leaf
  x:xs -> tinsert x (tFromList xs)