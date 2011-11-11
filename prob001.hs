sumDivisibleBy target n = let p = target `div` n in
  n * p * (p + 1) `div` 2

sumT = sumDivisibleBy 999

answer = (sumT 3) + (sumT 5) - (sumT 15)

main   = do print answer

