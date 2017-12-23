module Main where

import Graphics.SOE
import Lib
--import Shape
import SimpleGraphics
import Draw
import Perimeter

sh1 = Rectangle 3 2
sh2 = Ellipse 1 1.5
sh3 = RtTriangle 3 2
sh4 = Polygon [(-2.5, 2.5), (-1.5, 2.0), (-1.1, 0.2), (-1.7, -1.0), (-3.0, 0)]

type ColoredShapes = [(Color, Shape)]
shs :: ColoredShapes
shs = [(Red, sh1), (Blue, sh2), (Yellow, sh3), (Magenta, sh4)]

drawShapes :: Window -> ColoredShapes -> IO ()
drawShapes w css
  = sequence_ (map aux css)
    where aux (c, s) = drawInWindow w (withColor c (shapeToGraphic s))

conCircles = map circle [2.4, 2.1 .. 0.3]

coloredCircles
  = zip [Black, Blue, Green, Cyan, Red, Magenta, Yellow, White]
        conCircles

main :: IO ()
main = runGraphics (
  do w <- openWindow "Drawing Shapes" (xWin, yWin)
     drawShapes w coloredCircles
     putStrLn . show $ perimeter sh2
     spaceClose w
  )
