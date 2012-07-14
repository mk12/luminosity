-- Copyright 2012 Mitchell Kember.

module Misc where

import Data.List (minimumBy)

maybeMinBy :: (a -> a -> Ordering) -> [a] -> Maybe a
maybeMinBy _ [] = Nothing
maybeMinBy f xs = Just $ minimumBy f xs

-- toMaybe b x = if b then Just x else Nothing
--
-- import Control.Monad (guard)
-- mcondition :: (MonadPlus m) => Bool -> a -> m a
-- mcondition b x = guard b >> return x
