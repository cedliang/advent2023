{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.Text
import Data.Text.IO
import Prelude hiding (filter, head, last, readFile, words)

main :: IO ()
main = do
  fileLines <- words <$> readFile "src/main1/input.txt"
  let digits = filter isDigit . transformText <$> fileLines
  let sums = sumChars <$> digits
  print $ sum sums

sumChars :: Text -> Int
sumChars xs = 10 * read [head xs] + read [last xs]

transformText :: Text -> Text
transformText =
  replace "one" "o1e"
    . replace "two" "t2o"
    . replace "three" "t3e"
    . replace "four" "f4r"
    . replace "five" "f5e"
    . replace "six" "s6x"
    . replace "seven" "s7n"
    . replace "eight" "e8t"
    . replace "nine" "n9e"
