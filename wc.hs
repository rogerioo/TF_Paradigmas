import Data.Char (isSpace)
import Data.List (isInfixOf, maximumBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import System.Environment (getArgs)
import System.Process (readProcess)

wc :: [Char] -> [Char] -> [[Char]] -> [Char] -> [Char]
wc input "" [] output = if output == "" then wc input "" ["-l", "-w", "-c", ""] output else output
wc input "-c" (flag : flags) output = wc input flag flags (output ++ (show . length) input ++ " ")
wc input "--bytes" (flag : flags) output = wc input flag flags (output ++ (show . length) input ++ " ")
wc input "-m" (flag : flags) output = wc input flag flags (output ++ (show . length) input ++ " ")
wc input "--chars" (flag : flags) output = wc input flag flags (output ++ (show . length) input ++ " ")
wc input "-l" (flag : flags) output = wc input flag flags (output ++ (show . length . lines) input ++ " ")
wc input "--lines" (flag : flags) output = wc input flag flags (output ++ (show . length . lines) input ++ " ")
wc input "-L" (flag : flags) output = wc input flag flags (output ++ (show . length . maximumBy (comparing length) . lines) input ++ " ")
wc input "--max-line-length" (flag : flags) output = wc input flag flags (output ++ (show . length . maximumBy (comparing length) . lines) input ++ " ")
wc input "-w" (flag : flags) output = wc input flag flags (output ++ (show . length . words) input ++ " ")
wc input "--words" (flag : flags) output = wc input flag flags (output ++ (show . length . words) input ++ " ")
wc input option (flag : flags) output = wc input flag flags output

multipleWc :: [[Char]] -> [[Char]] -> [[Char]] -> [Char] -> [Char]
multipleWc (input : inputs) flags (name : names) output = multipleWc inputs flags names output ++ wc input "" flags "" ++ name ++ "\n"
multipleWc [] flags [] output = output

separeArgs :: [Char] -> [[Char]] -> [[Char]]
separeArgs "" output = output
separeArgs flags output = separeArgs (tail flags) (output ++ ["-" ++ [head flags]])

trimString :: [Char] -> [Char]
trimString = reverse . dropWhile isSpace . reverse

main :: IO ()
main = do
  args <- getArgs

  if "--help" `elem` args
    then do
      helpMessage <- readProcess "wc" ["--help"] ""
      putStrLn helpMessage
    else
      if "--version" `elem` args
        then do
          versionMessage <- readProcess "wc" ["--version"] ""
          putStrLn versionMessage
        else do
          files <- if any (isInfixOf "--files0-from=") args then readFile $ drop 14 (head args) else readFile $ head args

          rawString <- if any (isInfixOf "--files0-from=") args then mapM readFile $ splitOn "\0" files else return [files]

          let content = map trimString rawString

          let newArgs
                | length args > 1 && length (args !! 1) > 2 && (args !! 1) !! 1 /= '-' = separeArgs (args !! 1) []
                | otherwise = args

          let result
                | any (isInfixOf "--files0-from=") args = multipleWc content (tail newArgs ++ [""]) (splitOn "\0" files) "" ++ "\n" ++ wc (unlines content) "" (tail newArgs ++ [""]) "" ++ "total"
                | otherwise = wc (head content) "" (tail newArgs ++ [""]) "" ++ head args

          putStrLn result