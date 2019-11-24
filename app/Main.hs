module Main where

import           BF
import           System.Environment
import           System.Exit

main = do
  args <- getArgs
  parse args

parse ["-h"] = usage >> exit
parse ["-v"] = version >> exit
parse [] = getContents
parse (x:xs)
  | filefmt x == "bf" = toString <$> test
  | filefmt x == "l" = toString <$> test

filefmt :: String -> String
filefmt m = last $ wordsWhen (== '.') m

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'

usage = putStrLn "Usage: tac [-vh] [file ..]"

version = putStrLn "Haskell tac 0.1"

exit = exitWith ExitSuccess

die = exitWith (ExitFailure 1)

test :: IO (Mem Int)
test = run [Next, Inc, Out]
