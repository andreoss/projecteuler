-- getPolindrom :: Integer -> Integer -> Integer
-- getPolindrom iter n
--   | isPolindromic n = n
--   | iter > 100       = -1
--   | otherwise       = getPolindrom (iter + 1) (nextCheck n)


isPolindromic n = reverse n' == n'
  where n' = show n

nextCheck n = n + n'
   where n' = read (reverse $ show n) :: Integer

isLychrel iter n
  | iter > 50          = True
  | isPolindromic next = False
  | otherwise          = isLychrel (iter + 1) next
  where next = nextCheck n

lychrels = filter (isLychrel 0) [1..10000]

answer  = length lychrels
main    = print answer
