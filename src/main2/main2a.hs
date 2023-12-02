import Data.Char
import Data.List
import Data.List.Split

main :: IO ()
main = do
  fileLines <- init . splitOn "\n" <$> readFile "src/main2/input.txt"
  let games = splitOn ":" . filter (not . isSpace) <$> fileLines
  let gameNumbers = read . filter isDigit . head <$> games :: [Int]
  let gameRules = map (splitOn ";") $ last <$> games

  let gameRulesParsed = zip gameNumbers $ map gameReqs gameRules
  let legalGames = filter (legalGame . snd) gameRulesParsed

  print $ sum $ map fst legalGames
  where
    legalGame :: (Int, Int, Int) -> Bool
    legalGame (r, g, b) = r <= 12 && g <= 13 && b <= 14

gameReqs :: [String] -> (Int, Int, Int)
gameReqs xs =
  let occurrences = map calcOccurrences xs
   in foldl1 (\(r, g, b) (r', g', b') -> (max r r', max g g', max b b')) occurrences
  where
    calcOccurrences :: String -> (Int, Int, Int)
    calcOccurrences x =
      foldl
        ( \(r, g, b) req ->
            let count = read . filter isDigit $ req
             in if "red" `isInfixOf` req
                  then (r + count, g, b)
                  else
                    if "green" `isInfixOf` req
                      then (r, g + count, b)
                      else if "blue" `isInfixOf` req then (r, g, b + count) else (r, g, b)
        )
        (0, 0, 0)
        $ splitOn "," x
