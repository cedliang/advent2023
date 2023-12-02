import Data.Char

main :: IO ()
main = do
  fileLines <- words <$> readFile "src/main1/input.txt"
  let digits = map (filter isDigit) fileLines
  let sums = map sumChars digits
  print $ sum sums

sumChars :: String -> Int
sumChars xs = 10 * read [head xs] + read [last xs]
