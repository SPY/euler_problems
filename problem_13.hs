import System.Environment ( getArgs )

main :: IO ()
main = do
     [fn] <- getArgs
     cnt <- readFile fn
     let ans = take 10 $ show $ sum $ map read $ lines cnt
     putStrLn ans

