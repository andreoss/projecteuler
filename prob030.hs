import Data.Char (digitToInt)
digits :: Int -> [Int]
digits = map digitToInt . show 

sumOfNthPower n = sum . map (\ x -> x ^ n) . digits
sumOfFifthPower = sumOfNthPower 5

isGood 1        = False
isGood n        = n == (sumOfFifthPower n)

answer = sum $ filter isGood [1 .. 7 * 9^5] 
main   = do print answer

