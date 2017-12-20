module SimpleGraphics
  ( spaceClose
  , sierpinskiTri
  ) where

import Graphics.SOE

spaceClose :: Window -> IO ()
spaceClose w
  = do k <- getKey w
       if k == ' ' then closeWindow w
                   else spaceClose w

minSize = 8

fillTri :: Window -> Int -> Int -> Int -> IO ()
fillTri w x y size
  = drawInWindow w ( withColor Blue
      (polygon [(x, y), (x + size, y), (x, y - size), (x, y)]))

sierpinskiTri :: Window -> Int -> Int -> Int -> IO ()
sierpinskiTri w x y size
  = if size <= minSize
      then fillTri w x y size
      else let size2 = size `div` 2
        in do sierpinskiTri w x y size2
              sierpinskiTri w x (y - size2) size2
              sierpinskiTri w (x + size2) y size2
