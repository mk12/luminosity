-- Copyright 2012 Mitchell Kember

-- | A collection of useful miscellaneous functions which do not belong in any
-- other module.
module Luminosity.Misc where

import Data.List (minimumBy)

-- | Same as 'map', but with the arguments flipped.
for :: [a] -> (a -> b) -> [b]
for = flip map

-- | Same as 'minimumBy', but safe: returns 'Nothing' if the list is empty,
-- 'Just' the minimum otherwise.
maybeMinBy :: (a -> a -> Ordering) -> [a] -> Maybe a
maybeMinBy _ [] = Nothing
maybeMinBy f xs = Just $ minimumBy f xs
