module Main where
import qualified Data.List as DL

startText :: String
startText = "   CORA"
stopText :: String
stopText = "   MUSO"

startsWithText :: String -> String -> Bool
startsWithText str line =
  (length line >= startTextLen) && (str == take startTextLen line)
  where
      startTextLen = length str

isStartText :: String -> Bool
isStartText line = startsWithText startText line

isStopText :: String -> Bool
isStopText line = startsWithText stopText line

getStartTextIndices :: [String] -> [Int]
getStartTextIndices lines = DL.findIndices isStartText lines

getStopTextIndices :: [String] -> [Int]
getStopTextIndices lines = DL.findIndices isStopText lines

expandLines :: (Int, Int) -> [Int]
expandLines (x, y) = [x..y]

getBadLinesIndices :: [String] -> [Int]
getBadLinesIndices lines =
  let
    startIndices = getStartTextIndices lines
    stopIndices = getStopTextIndices lines
  in
    if (length startIndices == length stopIndices)
    then
      concat $ fmap expandLines (zip startIndices stopIndices)
    else
      error "lines do not match"
      
removeBadLines :: [String]-> [Int] -> [String]
removeBadLines lines linesIndices = removeBadLines_aux lines 0
  where
    removeBadLines_aux [] _ = []
    removeBadLines_aux (x:xs) aux
      | elem aux linesIndices = removeBadLines_aux xs (aux+1)
      | otherwise = x:(removeBadLines_aux xs (aux+1))


main :: IO ()
main = do
  f <- readFile "/home2/florin/Musonius.txt"
  let lineS = lines f
  putStrLn $ (unlines $ removeBadLines lineS (getBadLinesIndices lineS))
  -- putStrLn $ (show $ getBadLinesIndices lineS)
