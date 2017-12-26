module Animation where

import Shape
import Draw
import Picture
import Graphics.SOE hiding (Region)
import qualified Graphics.SOE as G (Region)
-- timeGetTime and Word are included in the module Graphics.SOE

type Animation a = Time -> a
type Time = Float

rubberBall :: Animation Shape
rubberBall t = Ellipse (sin t) (cos t)

revolvingBall :: Animation Region
revolvingBall t
  = let ball = Shape (Ellipse 0.2 0.2)
    in Translate (sin t, cos t) ball

planets :: Animation Picture
planets t
  = let p1 = Region Red (Shape (rubberBall t))
        p2 = Region Yellow (revolvingBall t)
    in p1 `Over` p2

tellTime :: Animation String
tellTime t = "The time is: " ++ show t

animate :: String -> Animation Graphic -> IO ()
animate title anim
  = runGraphics $
      do w <- openWindowEx title (Just (0, 0)) (Just (xWin, yWin)) drawBufferedGraphic (Just 16)
         t0 <- timeGetTime
         let loop
               = do t <- timeGetTime
                    let ft = intToFloat (word32ToInt (t - t0)) / 1000
                    setGraphic w (anim ft)
                    getWindowTick w
                    loop
         loop
