import System.Environment ( getArgs )
import Data.List (foldl1)

calc :: [[Int]] -> Int
calc = maximum . foldl1 zipRows

zipRows :: [Int] -> [Int] -> [Int]
zipRows t b = zipWith zipper b $ groupBy2 (head t : t)
	where zipper b (x,y) = b + (max x y)

groupBy2 :: [Int] -> [(Int, Int)]
groupBy2 [x,y] = [(x,y), (y,y)]
groupBy2 (x : y : rest) = (x, y) : groupBy2 (y : rest)

main :: IO ()
main = do
     [fileName] <- getArgs
     cnt <- readFile fileName
     let nums = map (map read) . map words . lines $ cnt
     putStrLn . show $ calc nums
