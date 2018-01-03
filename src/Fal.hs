module Fal where

import Graphics.SOE hiding (Region, Event)
import qualified Graphics.SOE as G (Region, Event, Point)
import Draw (pixelToInch)
import Animation (Time)
import Shape
import Picture
import GHC.Generics (Generic)
import Memo1

infixr 1 =>>, ->>
infixr 1 `untilB`, `switch`, `stepAccum`, `step`
infixl 0 .|.
infixr 4 <*, >*
infixr 3 &&*
infixr 2 ||*

type UserAction = G.Event

newtype Behavior a
  = Behavior (([Maybe UserAction], [Time]) -> [a])

newtype Event a
  = Event (([Maybe UserAction], [Time]) -> [Maybe a])

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

time :: Behavior Time
time = Behavior (\(_, ts) -> ts)

-- pure?
constB :: a -> Behavior a
constB x = Behavior (\_ -> repeat x)

-- <*>?
($*) :: Behavior (a -> b) -> Behavior a -> Behavior b
Behavior ff $* Behavior fb = Behavior (\uts -> zipWith ($) (ff uts) (fb uts))

lift0 :: a -> Behavior a
lift0 = constB

lift1 :: (a -> b) -> (Behavior a -> Behavior b)
lift1 f b1 = lift0 f $* b1

lift2 :: (a -> b -> c) -> (Behavior a -> Behavior b -> Behavior c)
lift2 f b1 b2 = lift1 f b1 $* b2

lift3 :: (a -> b -> c -> d) -> (Behavior a -> Behavior b -> Behavior c -> Behavior d)
lift3 f b1 b2 b3 = lift2 f b1 b2 $* b3

pairB :: Behavior a -> Behavior b -> Behavior (a, b)
pairB = lift2 (,)

fstB :: Behavior (a, b) -> Behavior a
fstB = lift1 fst
sndB :: Behavior (a, b) -> Behavior b
sndB = lift1 snd

paint :: Behavior Color -> Behavior Region -> Behavior Picture
paint = lift2 Region

red, blue, yellow, green, white, black :: Behavior Color
red  = lift0 Red
blue = lift0 Blue
yellow = lift0 Yellow
green = lift0 Green
white = lift0 White
black = lift0 Black

shape :: Behavior Shape -> Behavior Region
shape = lift1 Shape

ell, rec :: Behavior Float -> Behavior Float -> Behavior Region
ell x y = shape (lift2 Ellipse x y)
rec x y = shape (lift2 Rectangle x y)

translate :: (Behavior Float, Behavior Float) -> Behavior Region -> Behavior Region
translate (Behavior fx, Behavior fy) (Behavior fp)
  = Behavior (\uts -> zipWith3 aux (fx uts) (fy uts) (fp uts))
      where aux x y p = Translate (x, y) p

(>*), (<*) :: Ord a => Behavior a -> Behavior a -> Behavior Bool
(>*) = lift2 (>)
(<*) = lift2 (<)

(&&*), (||*) :: Behavior Bool -> Behavior Bool -> Behavior Bool
(&&*) = lift2 (&&)
(||*) = lift2 (||)

over :: Behavior Picture -> Behavior Picture -> Behavior Picture
over = lift2 Over

untilB, switch :: Behavior a -> Event (Behavior a) -> Behavior a
Behavior fb `untilB` Event fe
  = memoB $ Behavior (\uts@(us, ts) -> loop us ts (fe uts) (fb uts))
      where loop (_:us) (_:ts) ~(e:es) (b:bs)
              = b : case e of
                      Nothing             -> loop us ts es bs
                      Just (Behavior fb') -> fb' (us, ts)

memoB :: Behavior a -> Behavior a
memoB (Behavior fb) = Behavior (memo1 fb)

Behavior fb `switch` Event fe
  = memoB $ Behavior (\uts@(us, ts) -> loop us ts (fe uts) (fb uts))
      where loop (_:us) (_:ts) ~(e:es) (b:bs)
              = b : case e of
                      Nothing             -> loop us ts es bs
                      Just (Behavior fb') -> loop us ts es (fb' (us, ts))

lbp :: Event ()
lbp = Event (\(uas, _) -> map getlbp uas)
        where getlbp (Just (Button _ True True)) = Just ()
              getlbp _                           = Nothing

(=>>) :: Event a -> (a -> b) -> Event b
Event fe =>> f = Event (map (fmap f) . fe)

(->>) :: Event a -> b -> Event b
e ->> v = e =>> (\_ -> v)

while :: Behavior Bool -> Event ()
while (Behavior fb)
  = Event (\uts -> map aux (fb uts))
      where aux True  = Just ()
            aux False = Nothing

unique :: Eq a => Event a -> Event a
unique (Event fe)
  = Event (\uts -> aux (fe uts))
      where aux xs = zipWith remdup (Nothing : xs) xs
            remdup x y | x == y    = Nothing
                       | otherwise = y

when :: Behavior Bool -> Event ()
when = unique . while

integral :: Behavior Float -> Behavior Float
integral (Behavior fb)
  = Behavior (\uts@(us, t : ts) -> 0 : loop t 0 ts (fb uts))
      where loop t0 acc (t1:ts) (a:as)
              = let acc' = acc + (t1 - t0) * a
                in acc' : loop t1 acc' ts as

withElem :: Event a -> [b] -> Event (a, b)
withElem (Event fe) bs = Event (\uts -> loop (fe uts) bs)
  where loop (Just a : evs) (b:bs) = Just (a, b) : loop evs bs
        loop (Nothing : evs) bs    = Nothing : loop evs bs

withElem_ :: Event a -> [b] -> Event (b)
withElem_ e bs = e `withElem` bs =>> snd

(.|.) :: Event a -> Event a -> Event a
Event fe1 .|. Event fe2
  = Event (\uts -> zipWith aux (fe1 uts) (fe2 uts))
      where aux Nothing Nothing = Nothing
            aux (Just x) _ = Just x
            aux _ (Just y) = Just y

key :: Event Char
key = Event (\(uas, _) -> map getkey uas)
        where getkey (Just (Key ch True)) = Just ch
              getkey _                    = Nothing

snapshot :: Event a -> Behavior b -> Event (a, b)
Event fe `snapshot` Behavior fb
  = Event (\uts -> zipWith aux (fe uts) (fb uts))
      where aux (Just x) y = Just (x, y)
            aux Nothing _  = Nothing

snapshot_ :: Event a -> Behavior b -> Event b
snapshot_ e b = e `snapshot` b =>> snd

step :: a -> Event a -> Behavior a
a `step` e = constB a `switch` e =>> constB

stepAccum :: a -> Event (a -> a) -> Behavior a
a `stepAccum` e = b
  where b = a `step` (e `snapshot` b =>> uncurry ($))

counter = 0 `stepAccum` lbp ->> (+1)

mm :: Event Coordinate
mm = Event (\(uas, _) -> map getmm uas)
       where getmm (Just (MouseMove pt)) = Just (gPtToPt pt)
             getmm _                     = Nothing

gPtToPt :: G.Point -> Coordinate
gPtToPt (x, y) = (pixelToInch (x - 300)
                 ,pixelToInch (250 - y))

mouse :: (Behavior Float, Behavior Float)
mouse = (fstB m, sndB m)
          where m = (0, 0) `step` mm
