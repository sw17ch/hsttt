module Game.HSTTT.Util where

-- Some list utility functions.
allEq :: (Eq a) => [a] -> Bool
allEq [] = True
allEq (x:xs) = go x xs
  where
    go _ [] = True
    go v (a:as) | v == a = go v as
                | otherwise = False

mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex f = go 0
  where
    go _ [] = []
    go i (l:ls) = f l i : go (i + 1) ls
