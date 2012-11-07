isPrime n = all ( (/= 0) . mod n) $ takeWhile ( <= (floor . sqrt . fromIntegral $ n )) primes

primes = 2 : filter isPrime [3,5..]

answer = sum $ takeWhile (< 2000000) primes