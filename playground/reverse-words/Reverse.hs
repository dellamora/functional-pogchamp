module Main where

import Data.List (intercalate)

reverseWords :: String -> String
reverseWords = unwords . reverse . words . trim

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile (== ' ')

main :: IO ()
main = do
    print $ reverseWords "the sky is blue"
    print $ reverseWords "  hello world  "
    print $ reverseWords "a good example"