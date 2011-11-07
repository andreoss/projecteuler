digits :: Int -> [Int]
digits n
  | n < 10    = [n]
  | otherwise = (digits (n `quot` 10)) ++ [(n `mod` 10)]

square :: Int -> Int
square x = x * x


step :: Int -> Int
step x = foldr (+) 0 (map square (digits x))
-- step = sum . (map square) . digits


terminator :: Int -> Int
terminator n
  | n' == 1  = 1
  | n' == 89 = n
  | otherwise = terminator n'
  where n' = step n
