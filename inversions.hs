import Control.Monad

inversions :: [Int] -> Int
inversions = snd . countInversions

countInversions :: [Int] -> ([Int], Int)
countInversions []  = ([] , 0)
countInversions [x] = ([x], 0)
countInversions arr = let mid   = (length arr) `div` 2
                          left  = take mid arr
                          right = drop mid arr
                          (left'  , l) = countInversions $! left
                          (right' , r) = countInversions $! right
                          (whole  , w) = id $! countSplit mid left' right'
                      in (whole, l + r + w)

countSplit :: Int -> [Int] -> [Int] -> ([Int], Int)
countSplit _ [] y = (y, 0)
countSplit _ x [] = (x, 0)
countSplit len (x:xs) (y:ys)
        | x <= y    = ((x: left ), 0 + l)
        | otherwise = ((y: right), 0 + r)
            where leftSplit  = countSplit (len - 1) xs (y:ys)
                  rightSplit = countSplit len (x:xs) ys
                  left  = fst leftSplit
                  right = fst rightSplit
                  l     = snd leftSplit
                  r     = (snd rightSplit) + len

main = do
    nums <- liftM (map (read :: String -> Int) . lines) getContents
    print $ inversions nums

inversions' :: [Int] -> Int
inversions' [] = 0
inversions' (x:xs) = (length (filter (<x) xs)) + inversions' xs
