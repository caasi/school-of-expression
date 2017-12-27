module Main where

import System.Environment
import Graphics.SOE hiding (Region)
import SimpleGraphics
import Draw
import Picture
import Animation

-- chapter 3

ch3_pic1 = withColor Red (ellipse (150, 150) (300, 200))
ch3_pic2 = withColor Blue (polyline [(100, 50), (200, 50), (200, 250), (100, 250), (100, 50)])

-- chapter 4

sh1, sh2, sh3, sh4 :: Shape

sh1 = Rectangle 3 2
sh2 = Ellipse 1 1.5
sh3 = RtTriangle 3 2
sh4 = Polygon [(-2.5, 2.5), (-1.5, 2.0), (-1.1, 0.2), (-1.7, -1.0), (-3.0, 0)]

shs :: ColoredShapes
shs = [(Red, sh1), (Blue, sh2), (Yellow, sh3), (Magenta, sh4)]

type ColoredShapes = [(Color, Shape)]

drawShapes :: Window -> ColoredShapes -> IO ()
drawShapes w []
  = return ()
drawShapes w ((c, s) : cs)
  = do drawInWindow w (withColor c (shapeToGraphic s))
       drawShapes w cs

-- chapter 5

conCircles = map circle [2.4, 2.1..0.3]
coloredCircles = zip [Black, Blue, Green, Cyan, Red, Magenta, Yellow, White] conCircles

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
main = do
  args <- getArgs
  listMain args

listMain :: [String] -> IO ()
listMain []       = return ()
listMain (n : ns) = do
  case n of
    "3.0" ->
      runGraphics $
        do w <- openWindow "My First Graphics Program" (300, 300)
           drawInWindow w (text (100, 200) "Hello Graphics World")
           k <- getKey w
           closeWindow w
    "3.1" ->
      runGraphics $
        do w <- openWindow "My First Graphics Program" (300, 300)
           drawInWindow w (text (100, 200) "Hello Graphics World")
           spaceClose w
    "3.2" ->
      runGraphics $
        do w <- openWindow "Some Graphics Figures" (300, 300)
           drawInWindow w ch3_pic1
           drawInWindow w ch3_pic2
           spaceClose w
    "3.3" ->
      runGraphics $
        do w <- openWindow "Sierpinski's Triangle" (400, 400)
           sierpinskiTri w 50 300 256
           spaceClose w
    "4.0" ->
      runGraphics $
        do w <- openWindow "Drawing Shapes" (xWin, yWin)
           drawInWindow w (withColor Red (shapeToGraphic sh1))
           drawInWindow w (withColor Blue (shapeToGraphic sh2))
           spaceClose w
    "4.1" ->
      runGraphics $
        do w <- openWindow "Drawing Shapes" (xWin, yWin)
           drawShapes w shs
           spaceClose w
    "5.0" ->
      runGraphics $
        do w <- openWindow "Bull's Eye" (xWin, yWin)
           drawShapes w coloredCircles
           spaceClose w
    "10.0" ->
      draw "Region Test" pic3
    "10.1" ->
      draw2 "User Interactions" pic
    "13.0" ->
      animate "Animated Shape" (withColor Blue . shapeToGraphic . rubberBall)
    "13.1" ->
      animate "Animated Text" (text (100, 200) . tellTime)
    "13.2" ->
      animate "Animatde Region" (withColor Yellow . regionToGraphic . revolvingBall)
    "13.3" ->
      animate "Animate Picture" (picToGraphic . planets)
    "13.4" ->
      animateB "Revolving Ball Behavior" revolvingBallB
    "13.5" ->
      animateB "Flashing Ball" flashingBall
    "13.6" ->
      animateB "Lots of Flashing Balls" revolvingBalls
    "13.7" ->
      do animateB "kaleido1 (close window for next demo)" kaleido1
         animateB "kaleido2" kaleido2
    "kaleido1" ->
      animateB "kaleido1" kaleido1
    "kaleido2" ->
      animateB "kaleido2" kaleido2
    _ -> return ()
  listMain ns
