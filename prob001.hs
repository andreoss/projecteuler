module Problem_1
where

sumDivisibleBy target n = let p = target `div` n in
  n * p * (p + 1) `div` 2

sum' = sumDivisibleBy 999



main :: IO ()
main = print answer
   where
     answer :: Int
     answer = (sum' 3) + (sum' 5) - (sum' 15)
