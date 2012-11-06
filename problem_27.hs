import Data.List (maximumBy)
import Data.Function (on)
import Control.Arrow (first)

divisor n = (== 0) . mod n

isPrime n = (1 ==) . length $ filter (divisor n) $ [1..(floor . sqrt . fromIntegral $ n )]

eq a b x = x*x + a*x + b

range = [-1000..1000]

eqs = [(eq a b, (a,b)) | a <- range, b <- range]

makeSeq eq = takeWhile isPrime $ map eq [1..]

answer = uncurry (*) . snd $ maximumBy (compare `on` fst) $ map (first $ length . makeSeq) eqs

