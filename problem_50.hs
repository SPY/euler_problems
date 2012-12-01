n = 1000000

border :: Int -> Int
border = floor . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime n = all ( (/= 0) . mod n) $ takeWhile ( <= (border n )) primes

primes :: [Int]
primes = 2 : filter isPrime [3,5..]

pSums = 2 : zipWith (+) (tail primes) pSums

upperBound = head $ dropWhile (( < n) . snd)  $ zip [1..] pSums -- 547

windows = [547, 545..21]

groupByWindow n xs = take n xs : groupByWindow n (tail xs)

sequences x = filter (isPrime . snd) $ takeWhile ((< n) . snd)$ map (\w -> (w, sum w)) $ groupByWindow x primes

answer = head $ dropWhile ((== 0) . length) $ map sequences windows 



