digits :: Int -> [Int]
digits = (map negate) . (map (((-) (fromEnum '0') ) . fromEnum)) . show 


sumOfNthPower n = sum . (map (\x -> x ^ n)) . digits
sumOfFifthPower = sumOfNthPower 5

isGood 1        = False
isGood n        = n == (sumOfFifthPower n)

answer = sum $ filter isGood [1..7 * 9^5] -- since 7 * 9^5 < 9999999
main   = do print answer

