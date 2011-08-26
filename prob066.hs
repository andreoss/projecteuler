import Data.Ord  (comparing)
import Data.List (sortBy)

closestRoot  = floor . sqrt . fromIntegral

nonSquarable n =
  nS ^ 2 /= n where nS = closestRoot n

simpleSolution n =
  [a, b, k] where
    a = closestRoot n
    b = 1
    k = a ^ 2 - n

nextTriplet a b k n =
  let l = head $ sortBy (comparing
                  (\x -> abs(x ^ 2 - n)))
          $ filter (
        \x -> ( a + b * x ) `mod` k == 0
        ) [ 1 .. 1 + closestRoot n ]
      a' = (a * l + n * b) `div` abs k
      b' = (a     + l * b) `div` abs k
      k' = (l ^ 2 - n)     `div` k 
  in
   if k' == 1
   then [a',b',k']
   else nextTriplet a' b' k' n
    
  
chakravala n =
   let [a, b, k] = simpleSolution n in
   if k == 1 then [a, b, k]
   else
     nextTriplet a b k n


--map (\d -> chakravala d) $ filter nonSquarable [1 .. 1000]

answer = head
         $ last
         $ sortBy (comparing (!! 2))
         $ map (\d -> [d] ++ chakravala d)
         $ filter nonSquarable [1 .. 1000]

main = print answer
