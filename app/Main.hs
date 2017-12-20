module Main where

import Graphics.SOE
import Lib
import Shape
import SimpleGraphics

main :: IO ()
main = runGraphics (
  do w <- openWindow "My First Graphics Program" (400, 400)
     sierpinskiTri w 50 300 256
     spaceClose w
  )
