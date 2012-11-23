import Data.List (nub)
import qualified Data.Set as S

border :: Int -> Int
border = floor . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime n = all ( (/= 0) . mod n) $ takeWhile ( <= (border n )) primes

primes :: [Int]
primes = 2 : filter isPrime [3,5..]

trs :: [Int]
trs = 1 : zipWith (+) [2..] trs

divisors :: Int -> S.Set Int
divisors n = S.insert 1 $ S.unions $ map flater pds
    where flater (r, p) = S.insert r $ S.map (p*) $ divisors r
    	  divider p = (n `mod` p, (n `div` p, p))
          pds = map snd $ filter ((== 0) . fst) $ map divider $ takeWhile (<= (border n)) primes 

numDivisors :: Int -> Int
numDivisors = (+1) . S.size . divisors

answer = head $ dropWhile (( < 500  ) . fst) $ map (\n -> (numDivisors n, n)) trs