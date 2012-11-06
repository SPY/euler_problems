import Data.Function (on)
import Data.List (maximumBy)

chain n = takeWhile (/= 1) $ n : chain (if odd n then 3*n + 1 else n `div` 2) 

chains = zip [1..1000000] $ map ((+1) . length . chain) [1..1000000]

answer = fst $ maximumBy (compare `on` snd) $ chains

