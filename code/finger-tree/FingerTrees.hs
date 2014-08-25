{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Main where

import Data.FingerTree
import Data.Monoid

data Element a = Element a
instance Measured (Sum Int) (Element a) where
    measure _ = Sum 1
index :: FingerTree (Sum Int) (Element a) -> Int -> Maybe a
index tree k = 
  -- Discard the first k elements, and look at the leftmost remaining element.
  case viewl (dropUntil (> Sum k) tree) of
    -- If it's empty, we've dropped all elements,
    -- and this index was out of bounds to begin with.
    EmptyL -> Nothing
    Element x :< _ -> Just x

main1 =
  let tree = fromList $ map Element ['a'..'z'] in
    print $ index tree 13

data PrioritizedString = Str String
data Minimum = Minimum Int deriving Eq
instance Monoid Minimum where
    mempty = Minimum maxBound
    mappend (Minimum x) (Minimum y )= Minimum $ min x y

instance Measured Minimum SmallString where
    measure (Str s) = Minimum $ length s

main2 =
  let tree = fromList $ map Str ["Hello", "nope", "Long string"]
      minOf tree = 
        case viewl $ dropUntil (== measure tree) tree of
          EmptyL -> Nothing
          x :< _ -> Just x in
    print $ minOf tree

data Tree a = Branch Int (Tree a) (Tree a) | Leaf Int a

tree :: Tree Char
tree =
  Branch 4
    (Branch 2 (Leaf 1 'A') (Leaf 1 'B'))
    (Branch 2 (Leaf 1 'C') (Leaf 1 'D'))

-- Extract the annotation from a leaf or intermediate node.
annotation :: Tree a -> Int
annotation (Branch i _ _) = i
annotation (Leaf i _) = i

treeLookup :: Tree a -> Int -> Maybe a
treeLookup tree i = go tree 0
  where
    go (Leaf a x) seen =
      if a + seen == i + 1
      then Just x
      else Nothing
    go (Branch _ left right) seen =
      if annotation left + seen > i
      then go left seen
      else go right $ annotation left + seen

main3 = print $ map (treeLookup tree) [0..6]
main = main1

