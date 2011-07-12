import Data.List.Split
loadWords :: IO [String]
loadWords = do
  allWords <- readFile "p042_words.txt"
  let quotedWords = splitOn "," allWords
  return $ map (filter (/= '"')) quotedWords

triagleNum n = (n * (n + 1)) `div` 2

wordValue [] = 0
wordValue (x:xs) = ch x + wordValue xs
                   where
                     ch c = fromEnum c - fromEnum 'A' + 1

triagles = triagles' 1
  where triagles' x = triagleNum x : triagles' (x+1)

triaglesTo n = takeWhile (<= n) triagles
isTriangle :: Int -> Bool
isTriangle n = n == last (triaglesTo n)

main = do
  words <- loadWords
  let answer = length $ filter isTriangle (map wordValue words) in print answer
