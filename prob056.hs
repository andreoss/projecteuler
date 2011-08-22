import Data.Char
import Data.Ord  (comparing)
import Data.List (maximumBy)

sumOfDigits n = sum $ map digitToInt $ show n

maximumSum =  maximumBy (comparing snd)
   [ ([a,b] , sumOfDigits (a ^ b) ) | a <- [1..100], b <- [1..100]]
         

main = print (snd maximumSum)
