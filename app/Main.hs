module Main where

import Lib
import Shape

main :: IO ()
main = do
  let s1 = RtTriangle 3.0 4.0
  let s2 = Polygon [(0.0, 0.0), (8.0, 0.0), (0.0, 6.0)]
  putStrLn . show $ area s1
  putStrLn . show $ area s2
