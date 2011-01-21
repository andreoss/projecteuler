import Data.List (maximumBy)
import Data.Ord  (comparing)

isRightTriangle a b c = a^2 + b^2 == c^2  

variants n = [
  (a,b,c) | c <- [1..n], b<-[1..c],
  let a = n - b - c, a > 0 && isRightTriangle a b c ]

numOfVariants = length . variants

answer = fst
         $ maximumBy (comparing snd)
         $ map (\n -> (n, numOfVariants n)) [1..1000]
main   = do print answer
