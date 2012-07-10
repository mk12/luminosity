-- Copyright 2012 Mitchell Kember.

module Misc where

import Data.List (minimumBy)

maybeMinBy :: (a -> a -> Ordering) -> [a] -> Maybe a
maybeMinBy _ [] = Nothing
maybeMinBy f xs = Just $ minimumBy f xs
