module Main where

import           Data.Char

main :: IO ()
main = return ()

solve s = map f [0 .. length s - 1] where
    f i
        | 'a' <= s !! i &&  s !! i <= 'z' = chr $ (ord (s !! i) - ord 'a' - 24 - 8 * i) `mod` 26 + ord 'a'
        | 'A' <= s !! i &&  s !! i <= 'Z' = chr $  (ord (s !! i) - ord 'A' - 24 - 8 * i) `mod` 26 + ord 'A'
        | otherwise = s !! i


