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
                          (whole  , w) = id $! countSplit left' right'
                      in (whole, l + r + w)

countSplit :: [Int] -> [Int] -> ([Int], Int)
countSplit [] y = (y, 0)
countSplit x [] = (x, 0)
countSplit (x:xs) (y:ys)
        | x <= y    = ((x: left ), 0 + l)
        | otherwise = ((y: right), 0 + r)
            where leftSplit  = countSplit xs (y:ys)
                  rightSplit = countSplit (x:xs) ys
                  left  = fst leftSplit
                  right = fst rightSplit
                  l     = snd leftSplit
                  r     = (snd rightSplit) + length((x: xs))

main = do
    nums <- liftM (map (read :: String -> Int) . lines) getContents
    print $ inversions nums

inversions' :: [Int] -> Int
inversions' [] = 0
inversions' (x:xs) = (length (filter (<x) xs)) + inversions' xs
