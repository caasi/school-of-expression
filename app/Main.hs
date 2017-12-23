module Main where

import Graphics.SOE
import Lib
import Shape
import SimpleGraphics
import Draw

sh1 = Rectangle 3 2
sh2 = Ellipse 1 1.5
sh3 = RtTriangle 3 2
sh4 = Polygon [(-2.5, 2.5), (-1.5, 2.0), (-1.1, 0.2), (-1.7, -1.0), (-3.0, 0)]

type ColoredShapes = [(Color, Shape)]
shs :: ColoredShapes
shs = [(Red, sh1), (Blue, sh2), (Yellow, sh3), (Magenta, sh4)]

drawShapes :: Window -> ColoredShapes -> IO ()
drawShapes w [] = return ()
drawShapes w ((c, s) : cs)
  = do drawInWindow w (withColor c (shapeToGraphic s))
       drawShapes w cs

main :: IO ()
main = runGraphics (
  do w <- openWindow "Drawing Shapes" (xWin, yWin)
     drawShapes w shs
     spaceClose w
  )
