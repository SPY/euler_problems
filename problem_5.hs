-- exclude all nums which are multipliers of another nums from [1..20]
-- filter prime nums from remainder [11,13,17,19]
-- our target is num of form : 11*13*17*19*x

vars = map ((11*13*17*19)*) [1..]

divided n = all (== 0) $ zipWith mod (repeat n) [12,14,15,16,18,20]

answer = head $ filter divided vars