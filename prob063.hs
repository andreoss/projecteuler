

powers b = takeWhile (\(_,_,n) -> n == b)
           $ dropWhile (\(_,_,n) -> n/=b)
           $ map  (
  (\ (x,xb) -> (x, b, length $ show xb)) . (\x -> (x, x ^ b))) [1..]


answer = length
         $ concatMap powers [1..21]

main = print answer
