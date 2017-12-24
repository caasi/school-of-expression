module Main where

import Graphics.SOE hiding (Region)
import Lib
import Draw
import Picture

-- user interactions
pictToList :: Picture -> [(Color, Region)]
pictToList EmptyPic = []
pictToList (Region c r) = [(c, r)]
pictToList (p1 `Over` p2) = pictToList p1 ++ pictToList p2

openWindow' :: Title -> Size -> IO Window
openWindow' name size = openWindowEx name Nothing (Just size) drawBufferedGraphic Nothing

adjust :: [(Color, Region)] -> Coordinate -> (Maybe (Color, Region), [(Color, Region)])
adjust [] p = (Nothing, [])
adjust ((c, r) : regs) p
  = if r `containsR` p then (Just (c, r), regs)
    else let (hit, rs) = adjust regs p
         in (hit, (c, r) : rs)

adjust' :: [(Color, Region)] -> Coordinate -> (Maybe (Color, Region), [(Color, Region)])
adjust' regs p
  = case (break (\(_, r) -> r `containsR` p) regs) of
      (top, hit : rest) -> (Just hit, top ++ rest)
      (_, []) -> (Nothing, regs)

loop :: Window -> [(Color, Region)] -> IO ()
loop w regs
  = do clearWindow w
       sequence_ [drawRegionInWindow w c r | (c, r) <- reverse regs]
       (x, y) <- getLBP w
       case (adjust regs (pixelToInch (x - xWin2), pixelToInch (yWin2 - y))) of
         (Nothing, _)        -> closeWindow w
         (Just hit, newRegs) -> loop w (hit : newRegs)

draw2 :: String -> Picture -> IO ()
draw2 s p
  = runGraphics $
    do w <- openWindow' s (xWin, yWin)
       loop w (pictToList p)

-- 不定義奇怪的 infix oprator 是怕讀者還不習慣嗎？
xUnion :: Region -> Region -> Region
p1 `xUnion` p2 = (p1 `Intersect` Complement p2) `Union` (p2 `Intersect` Complement p1)

r1 = Shape (Rectangle 3 2)
r2 = Shape (Ellipse 1 1.5)
r3 = Shape (RtTriangle 3 2)
r4 = Shape (Polygon [(-2.5, 2.5), (-3.0, 0), (-1.7, -1.0), (-1.1, 0.2), (-1.5, 2.0)])

reg1 = r3 `xUnion` (r1 `Intersect` Complement r2 `Union` r4)
pic1 = Region Blue reg1

reg2 = let circle = Shape (Ellipse 0.5 0.5)
           square = Shape (Rectangle 1 1)
       in (Scale (2, 2) circle)
          `Union` (Translate (1, 0) square)
          `Union` (Translate (-1, 0) square)
pic2 = Region Yellow (Translate (0, -1) reg2)

pic3 = pic2 `Over` pic1

-- pics for user interactions
p1, p2, p3, p4 :: Picture
p1 = Region Red r1
p2 = Region Blue r2
p3 = Region Green r3
p4 = Region Yellow r4

pic :: Picture
pic = foldl Over EmptyPic [p1, p2, p3, p4]

main :: IO ()
main = draw2 "Picture Click Test" pic
