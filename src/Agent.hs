module Agent where

import Graphics.UI.Gtk
import Control.Monad.IO.Class      -- for liftIO
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo as RendCairo
import Control.Monad                --  for  ForM_
import Data.Text (pack)

import Data.IORef
import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Concurrent (forkIO, threadDelay)
import Graphics.UI.Gtk.Gdk.Events (eventButton)


type Position = (Double, Double)
type Obstacle = (Double, Double, Double, Double, String)

class Eq p => Agent p where
    -- DrawAgent
    drawAgent :: p -> Render ()
    drawAgent (x, y) = do
        setSourceRGB 1.0 0.0 0.0 -- Rojo
        arc x y 10 0 (2 * pi)
        fill

    -- Movimiento A B
    agentMovent :: DrawingArea -> IORef Position -> Position -> [Obstacle] -> IO ()
    agentMovent paintArea agentPosRef targetPos obstacles = do
        currentPos <- readIORef agentPosRef
        case aStar currentPos targetPos obstacles of
        Just path -> do
            -- Temporizador para actualizar la posición del agente
            let updatePosition (x, y) (tx, ty) = 
                let dx = if x < tx then 1 else if x > tx then -1 else 0
                    dy = if y < ty then 1 else if y > ty then -1 else 0
                in (x + dx, y + dy)
            let moveAgent [] = return ()
                moveAgent (pos:ps) = do
                writeIORef agentPosRef pos
                postGUIAsync $ widgetQueueDraw paintArea
                threadDelay 50000
                moveAgent ps
            forkIO (moveAgent path) >> return ()
        Nothing -> putStrLn "No path found"
    
    -- Kill agent
    
        

    
    
    


    
