module ReactiveMain where

import Prelude hiding ((<*))
import Picture (picToGraphic)
import Fal
import Reactimate

paddleball vel = walls `over` paddle `over` pball vel

walls = let upper = paint blue (translate (0, 1.7) (rec 4.4 0.05))
            left  = paint blue (translate (-2.2, 0) (rec 0.05 3.4))
            right = paint blue (translate (2.2, 0) (rec 0.05 3.4))
        in upper `over` left `over` right

paddle = paint red (translate (fst mouse, -1.7) (rec 0.5 0.05))

pball vel
  = let xvel    = vel `stepAccum` xbounce ->> negate
        xpos    = integral xvel
        xbounce = when (xpos >* 2 ||* xpos <* -2)
        yvel    = vel `stepAccum` ybounce ->> negate
        ypos    = integral yvel
        ybounce = when
                    (   ypos >* 1.5
                    ||* ypos `between` (-2.0, -1.5)
                    &&* fst mouse `between` (xpos - 0.25, xpos + 0.25)
                    )
    in paint yellow (translate (xpos, ypos) (ell 0.2 0.2))

x `between` (a, b) = x >* a &&* x <* b

main :: IO ()
main = reactimate "Paddleball!!" (paddleball 2.0) (return . picToGraphic)
