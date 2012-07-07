-- Copyright 2012 Mitchell Kember.

module Misc where

import Data.List (minimumBy)

maybeMinBy :: (a -> a -> Ordering) -> [a] -> Maybe a
maybeMinBy _ [] = Nothing
maybeMinBy f xs = Just $ minimumBy f xs


-- Render.hs
-- non exanded form or horizontal offset from center for ortho rayForPixel
-- u = resX / resY * (-k + x / resX * scale)


-- Intersect.hs

-- intersection :: Ray -> Surface -> Maybe Vector
-- intersection r s = fmap (`extend` r) $ intersect r s
-- intersection = liftM2 (.) (fmap . flip extend) intersect
