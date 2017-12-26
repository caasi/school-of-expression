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

-- Type Classes to the Rescue
newtype Behavior a = Beh (Time -> a)

animateB :: String -> Behavior Picture -> IO ()
animateB s (Beh pf) = animate s (picToGraphic . pf)

instance Eq (Behavior a) where
  a1 == a2 = error "Can't compare behaviors."

instance Show (Behavior a) where
  showsPrec n a1 = error "<< Behavior >>"

instance Num a => Num (Behavior a) where
  (+)         = lift2 (+)
  (*)         = lift2 (*)
  negate      = lift1 negate
  abs         = lift1 abs
  signum      = lift1 signum
  fromInteger = lift0 . fromInteger

instance Fractional a => Fractional (Behavior a) where
  (/)          = lift2 (/)
  fromRational = lift0 . fromRational

instance Floating a => Floating (Behavior a) where
  pi = lift0 pi
  sqrt = lift1 sqrt
  exp = lift1 exp
  log = lift1 log
  sin = lift1 sin
  cos = lift1 cos
  tan = lift1 tan
  asin = lift1 asin
  acos = lift1 acos
  atan = lift1 atan
  sinh = lift1 sinh
  cosh = lift1 cosh
  tanh = lift1 tanh
  asinh = lift1 asinh
  acosh = lift1 acosh
  atanh = lift1 atanh

-- 很像 return
lift0 :: a -> Behavior a
lift0 x = Beh (\t -> x)

lift1 :: (a -> b) -> (Behavior a -> Behavior b)
lift1 f (Beh a) = Beh (\t -> f (a t))

lift2 :: (a -> b -> c) -> (Behavior a -> Behavior b -> Behavior c)
lift2 g (Beh a) (Beh b) = Beh (\t -> g (a t) (b t))

lift3 :: (a -> b -> c -> d) -> (Behavior a -> Behavior b -> Behavior c -> Behavior d)
lift3 g (Beh a) (Beh b) (Beh c) = Beh (\t -> g (a t) (b t) (c t))

-- 很像從 State Monad 裡面拿 state 出來（？）
time :: Behavior Time
time = Beh (\t -> t)

-- 很像 Monoid
class Combine a where
  empty :: a
  over :: a -> a -> a

instance Combine [a] where
  empty = []
  over  = (++)

instance Combine (Fun a) where
  empty              = Fun id
  Fun a `over` Fun b = Fun (a . b)

newtype Fun a = Fun (a -> a)

instance Combine Picture where
  empty = EmptyPic
  over  = Over

instance Combine a => Combine (Behavior a) where
  empty = lift0 empty
  over  = lift2 over

overMany :: Combine a => [a] -> a
overMany = foldr over empty

reg                                = lift2 Region
shape                              = lift1 Shape
ell                                = lift2 Ellipse
red                                = lift0 Red
yellow                             = lift0 Yellow
translate (Beh a1, Beh a2) (Beh r) = Beh (\t -> Translate (a1 t, a2 t) (r t))

revolvingBallB :: Behavior Picture
revolvingBallB
  = let ball = shape (ell 0.2 0.2)
    in reg red (translate (sin time, cos time) ball)

(>*) :: Ord a => Behavior a -> Behavior a -> Behavior Bool
(>*) = lift2 (>)

ifFun :: Bool -> a -> a -> a
ifFun p c a = if p then c else a

cond :: Behavior Bool -> Behavior a -> Behavior a -> Behavior a
cond = lift3 ifFun

flash :: Behavior Color
flash = cond (sin time >* 0) red yellow

timeTrans :: Behavior Time -> Behavior a -> Behavior a
timeTrans (Beh f) (Beh a) = Beh (a . f)

flashingBall :: Behavior Picture
flashingBall
  = let ball = shape (ell 0.2 0.2)
    in reg (timeTrans (8 * time) flash) (translate (sin time, cos time) ball)

revolvingBalls :: Behavior Picture
revolvingBalls
  = overMany [timeTrans (lift0 (t * pi/4) + time) flashingBall | t <- [0..7]]
