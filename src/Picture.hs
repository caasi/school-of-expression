module Picture
  ( Picture
    ( Region
    , Over
    , EmptyPic
    )
  , Color
    ( Black
    , Blue
    , Green
    , Cyan
    , Red
    , Magenta
    , Yellow
    , White
    )
  , regionToGRegion
  , shapeToGRegion
  , regionToGraphic
  , picToGraphic
  , drawRegionInWindow
  , drawPic
  , draw
  , spaceClose
  , module Region
  ) where

import Draw
import Region
import Graphics.SOE hiding (Region)
import qualified Graphics.SOE as G (Region)

data Picture
  = Region Color Region
  | Picture `Over` Picture
  | EmptyPic
  deriving Show

regionToGraphic :: Region -> Graphic
regionToGraphic = drawRegion . regionToGRegion

picToGraphic :: Picture -> Graphic
picToGraphic (Region c r) = withColor c (regionToGraphic r)
picToGraphic (p1 `Over` p2) = picToGraphic p1 `overGraphic` picToGraphic p2
picToGraphic EmptyPic = emptyGraphic

drawRegionInWindow :: Window -> Color -> Region -> IO ()
drawRegionInWindow w c r
  = drawInWindow w (withColor c (regionToGraphic r))

drawPic :: Window -> Picture -> IO ()
drawPic w (Region c r) = drawRegionInWindow w c r
drawPic w (p1 `Over` p2)
  = do drawPic w p2
       drawPic w p1
drawPic w EmptyPic = return ()

draw :: String -> Picture -> IO ()
draw s p
  = runGraphics $
    do w <- openWindow s (xWin, yWin)
       drawPic w p
       spaceClose w

-- transform a region to a Graphics.SOE.Region
regionToGRegion :: Region -> G.Region
regionToGRegion r = regToGReg (0, 0) (1, 1) r

regToGReg :: Vector -> Vector -> Region -> G.Region
regToGReg loc sca (Shape s) = shapeToGRegion loc sca s
regToGReg loc (sx, sy) (Scale (u, v) r) = regToGReg loc (sx * u, sy * v) r
regToGReg (lx, ly) (sx, sy) (Translate (u, v) r)
  = regToGReg (lx + u * sx, ly + v * sy) (sx, sy) r
regToGReg loc sca Empty = createRectangle (0, 0) (0, 0)
regToGReg loc sca (r1 `Union` r2) = primGReg loc sca r1 r2 orRegion
regToGReg loc sca (r1 `Intersect` r2) = primGReg loc sca r1 r2 andRegion
regToGReg loc sca (Complement r) = primGReg loc sca winRect r diffRegion

primGReg loc sca r1 r2 op
  = let gr1 = regToGReg loc sca r1
        gr2 = regToGReg loc sca r2
    in op gr1 gr2

winRect :: Region
winRect = Shape (Rectangle (pixelToInch xWin) (pixelToInch yWin))

shapeToGRegion :: Vector -> Vector -> Shape -> G.Region
shapeToGRegion (lx, ly) (sx, sy) s
  = case s of
      Rectangle s1 s2
        -> createRectangle (trans (-s1/2, -s2/2)) (trans (s1/2, s2/2))
      Ellipse r1 r2
        -> createEllipse (trans (-r1, -r2)) (trans (r1, r2))
      Polygon vs
        -> createPolygon (map trans vs)
      RtTriangle s1 s2
        -> createPolygon (map trans [(0, 0), (s1, 0), (0, s2)])
    where trans :: Vertex -> Point -- 和 Draw 裡面的 trans 不一樣喔
          trans (x, y) = ( xWin2 + inchToPixel (lx + x * sx)
                         , yWin2 - inchToPixel (ly + y * sy)
                         )

type Vector = (Float, Float)
