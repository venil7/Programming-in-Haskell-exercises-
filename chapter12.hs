-- 1
data Tree a = Leaf a | Node (Tree a) a (Tree a)
      deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) Tree a -> Tree b
  fmap f (Leaf a)     = Leaf (f a)
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

-- 2
instance Functor ((->) a) where
  -- fmap:: (b -> c) (a -> b) -> (a -> c)
  fmap = (.)

-- 3
instance Applicative ((->) a) where
  -- pure :: b -> (a -> b)
  pure = const
  <*> :: (a -> b -> c) -> (a -> b) -> (a -> c)