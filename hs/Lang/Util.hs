module Lang.Util(split) where

split :: Eq a => a -> [a] -> [[a]]
split y xs = splitAccum y xs id
    where splitAccum _ [] zs = [zs []]
          splitAccum y (x:xs) zs
                         | y == x = zs [] : splitAccum y xs id
                         | otherwise = splitAccum y xs (zs . (x:))
