-- Copyright 2012 Mitchell Kember.

module Misc where

import Data.List (minimumBy)

-- Map a function over a functor with fmap using the reverse argument order.
-- This provides the same counterpart that forM provides to mapM for monads.
for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

-- Safely return the least element of the list using the comparison function.
maybeMinBy :: (a -> a -> Ordering) -> [a] -> Maybe a
maybeMinBy _ [] = Nothing
maybeMinBy f xs = Just $ minimumBy f xs
