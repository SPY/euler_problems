import Data.Char (digitToInt)

fact n = product [1..n]

answer = sum . map digitToInt . show . fact $ 100
