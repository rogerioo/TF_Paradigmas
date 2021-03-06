import Data.Char (isSpace)
import Data.List (isInfixOf, maximumBy)
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

separeArgs :: [[Char]] -> [[Char]] -> [[Char]] -> ([[Char]], [[Char]])
separeArgs [] files flags = (files, flags)
separeArgs (arg : args) files flags =
  if "-" `isInfixOf` arg
    then separeArgs args files (flags ++ [arg])
    else separeArgs args (files ++ [arg]) flags

separeFlags :: Foldable t => t [Char] -> [[Char]] -> [[Char]]
separeFlags flags output =
  foldl
    ( \output flag ->
        output
          ++ if "-" `isInfixOf` flag && not ("--" `isInfixOf` flag)
            then map ("-" ++) (splitAll $ tail flag)
            else [flag]
    )
    output
    flags

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

trimString :: [Char] -> [Char]
trimString = reverse . dropWhile isSpace . reverse

splitByCharacter :: Char -> [Char] -> [String]
splitByCharacter _ "" = []
splitByCharacter delimiterChar inputString = foldr f [""] inputString
  where
    f :: Char -> [String] -> [String]
    f currentChar allStrings@(partialString : handledStrings)
      | currentChar == delimiterChar = "" : allStrings
      | otherwise = (currentChar : partialString) : handledStrings

splitAll :: [a] -> [[a]]
splitAll = fmap (: [])

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
          let split = separeArgs args [] []
              files = removeDuplicates $ fst split
              flags = removeDuplicates $ separeFlags (snd split) []

          filesNames <-
            if any ("--files0-from=" `isInfixOf`) args
              then do
                filesPath <- readFile $ drop 14 (head args)
                return (splitByCharacter '\0' filesPath)
              else return files

          rawString <- mapM readFile filesNames

          let content = map trimString rawString

          let result =
                multipleWc content (flags ++ [""]) filesNames ""
                  ++ ( if length files > 1
                         then
                           "\n"
                             ++ wc (unlines content) "" (flags ++ [""]) ""
                             ++ "total\n"
                         else ""
                     )

          putStr result