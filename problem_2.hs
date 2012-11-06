let fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

answer = sum $ filter even $ takeWhile (< 4000000) $ fibs