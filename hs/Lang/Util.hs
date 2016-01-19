module Lang.Util(split) where

split :: Eq a => a -> [a] -> [[a]]
split y xs = splitAccum y xs []
    where splitAccum _ [] zs = [zs]
          splitAccum y (x:xs) zs
                         | y == x = zs : splitAccum y xs []
                         | otherwise = splitAccum y xs (x:zs)
