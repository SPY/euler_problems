import Data.List (permutations, nub)
import Data.Char (chr)

divisor n = (== 0) . mod n

isPrime n = (1 ==) . length $ filter (divisor n) $ [1..(floor . sqrt . fromIntegral $ n )]

ds = [0..9]

quads = filter (not . divBy3) $ do 
      x <- tail ds
      y <- ds
      z <- ds
      w <- [1,3,7,9]
      return [x,y,z,w]

toNum n = sum $ zipWith (*) n $ reverse $ take (length n) $ map (10 ^) [0..]

divBy3 n = mod (sum n) 3 == 0

correct n = head n > 0 && last n `elem` [1,3,7,9]

perms = filter correct . permutations

groupPerms ps = do
	   p1 <- ps
	   p2 <- tail ps
	   p3 <- drop 2 ps
	   if p1 < p2 && p2 < p3
	   then return [p1,p2,p3]
	   else []

isSeq n = (n3 - n2) == (n2 - n1) where [n1, n2, n3] = map toNum n

isCorrectSeq l = isSeq l && all (isPrime . toNum) l 

answer = map (chr . (+48)) $ concat $ (!! 1) $ nub $ filter isCorrectSeq $ concatMap (groupPerms . perms) $ quads

