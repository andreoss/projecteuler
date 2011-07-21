import Data.List (nub)
import qualified Data.Set as S


isPandigital ds = S.fromList ds == S.fromList "123456789"
    


isGood a b = let p = a * b
                 p' = show p
                 a' = show a
                 b' = show b
                 x  = a' ++ b' ++ p'
                 in length x == 9 && isPandigital x

good =  [ [a,b] | a <- [1..2000], b<-[1..a], isGood a b]
answer = sum $ nub $ map product good

main = do print answer
