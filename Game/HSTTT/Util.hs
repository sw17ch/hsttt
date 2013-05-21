module Game.HSTTT.Util where

-- |True when all elements of the list are the equal.
allEq :: (Eq a) => [a] -> Bool
allEq [] = True
allEq (x:xs) = go x xs
  where
    go _ [] = True
    go v (a:as) | v == a = go v as
                | otherwise = False

-- |Maps a function across a list along with the index of the current element.
mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex f = go 0
  where
    go _ [] = []
    go i (l:ls) = f l i : go (i + 1) ls
