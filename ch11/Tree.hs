module Tree
( Tree(..)
, singleton
, treeInsert
, YesNo(..)
) where

import Data.Monoid
import qualified Data.Foldable as F

class YesNo a where
    yesno :: a -> Bool

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x Empty Empty

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x Empty = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x Empty = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

instance YesNo (Tree a) where
    yesno Empty = False
    yesno _     = True

instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r

treeToList :: Tree a -> [a]
treeToList = F.foldMap (\x -> [x])
