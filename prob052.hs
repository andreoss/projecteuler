
import Data.List
digits n = sort $ show n

sameDigits n = all (== digits n) 
   $ map (digits . (* n)) [2..6]

permuted = [ x | x <- [1..],  sameDigits x ]
answer = head permuted
main   = print answer

