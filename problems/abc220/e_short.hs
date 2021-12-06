module Main where

main = do
  ln <- getLine
  putStrLn $ solve ln
main_=interact$show.(\[n,d]->let
  x=iterate((`mod`m).(*2))1!!(d-1)
  m=998244353
  f 1=0
  f y=(2*f(y-1)+x*if d<y then d+3 else max(2*y-d-1)0)`mod`m in f n).map read.words

solve :: String -> String
solve = show.(\[n,d]->let
  x=iterate((`mod`m).(*2))1!!(d-1)
  m=998244353
  f 1=0
  f n=(2*f(n-1)+x*if d<=n-1 then d+3 else max(2*n-d-1)0)`mod`m in f n).map read.words
