module Reactimate where

import Fal
import Graphics.SOE hiding (Region, Event)
import qualified Graphics.SOE as G (Region, Event)
import Draw (xWin, yWin, intToFloat)
import Animation (Time)
import Control.Concurrent.Chan

reactimate :: String -> Behavior a -> (a -> IO Graphic) -> IO ()
reactimate title fragProg toGraphic
  = runGraphics $
      do w <- openWindowEx title (Just (0, 0)) (Just (xWin, yWin)) drawBufferedGraphic (Just 16)
         (user, addEvents) <- windowUser w
         addEvents
         let drawPic (Just p)
               = do g <- toGraphic p
                    setGraphic w g
                    addEvents
                    getWindowTick w
             drawPic Nothing
               = return ()
         mapM_ drawPic (runEvent (sample `snapshot_` fragProg) user)

runEvent (Event fe) u = fe u

sample :: Event ()
sample = Event (\(us, _) -> map aux us)
  where aux Nothing  = Just ()
        aux (Just _) = Nothing

windowUser :: Window -> IO (([Maybe UserAction], [Time]), IO ())
windowUser w
  = do (evs, addEv) <- makeStream
       t0 <- timeGetTime
       let loop rt
             = do mev <- maybeGetWindowEvent w
                  case mev of
                    Nothing -> return ()
                    Just e  -> do addEv (Just e, rt)
                                  loop rt
       let addEvents
             = do t <- timeGetTime
                  let rt = w32ToTime (t - t0)
                  loop rt
                  addEv (Nothing, rt)
       return ((map fst evs, map snd evs), addEvents)

w32ToTime t = intToFloat (word32ToInt t) / 1000

makeStream :: IO ([a], a -> IO ())
makeStream
  = do ch <- newChan
       contents <- getChanContents ch
       return (contents, writeChan ch)
