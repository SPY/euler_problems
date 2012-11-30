
year n = concatMap (\n -> [1..n]) monthes
     where monthes = [31, if leap then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
     	   leap = n `mod` 400 == 0 || (n `mod`4 == 0 && not (n `mod` 100 == 0))

years = zip (cycle [1..7]) $ concatMap year [1900..2000]

answer = length . filter ((1,7) == ) $ drop 365 years



 
 