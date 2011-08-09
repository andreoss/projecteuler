import Data.List (permutations, sort)

isPrime :: Integral a => a -> Bool
isPrime n
  | n == 1     = False
  | n == 2     = True
  | even n    = False
  | otherwise = null $ filter (\ x -> rem n x == 0) [3 .. l]
  		where l = round $ sqrt $ fromIntegral n

pandigitals = foldr (++) []
  [ p |
    d <- digits,
    let p = reverse 
            $ sort 
            $ map (\x -> read x :: Integer) 
            $ permutations ['1'..d] ]
  where digits = ['9','8'..'1']

answer     = head $ filter isPrime pandigitals
main       = do print answer
