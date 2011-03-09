import Data.List (maximumBy)
import Data.Ord  (comparing)
import qualified Data.Set as S

nums' 0 m _  = []  
nums' n m memo
  | (n,m) `S.member` memo = []
  | n `mod` m == n = 0 : nums' (n * 10) m memoN 
  | otherwise     = n `div` m : nums' (n `mod` m) m memoN
  where memoN = S.fromList [(n,m)] `S.union` memo
  

nums m = nums' 1 m S.empty

answer = fst
         $ maximumBy (comparing snd)
         $ map (\d -> (d , length $ nums d)) [1..1000]

main = do print answer
