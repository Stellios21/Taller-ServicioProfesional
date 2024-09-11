module AgentWindow where

    import Graphics.UI.Gtk
    import Control.Monad.IO.Class      -- for liftIO
    import Graphics.Rendering.Cairo
    import Graphics.Rendering.Cairo as RendCairo
    import Control.Monad                --  for  ForM_
    import Data.Text (pack)

    import Graphics.UI.Gtk.Gdk.Screen
    import Graphics.UI.Gtk.Gdk.Pixbuf

    import Data.IORef

    import AStar
    import Agent



    -- Dibuja los items de la izquierda
    drawShapes :: Render ()
    drawShapes = do
        setSourceRGB 1.0 1.0 0.0  -- Amarillo
        RendCairo.rectangle 10 10 30 30
        fill

        setSourceRGB 0.0 1.0 0.0  -- Verde
        moveTo 25 60
        lineTo 40 90
        lineTo 10 90
        closePath
        fill

        setSourceRGB 1.0 0.5 0.0  -- Naranja
        arc 25 130 15 0 (2 * pi)
        fill

        setSourceRGB 0.0 0.0 1.0  -- Azul
        moveTo 25 170
        lineTo 45 190
        lineTo 35 210
        lineTo 15 210
        lineTo 5 190
        closePath
        fill

        setSourceRGB 0.5 0.0 0.5  -- Morado
        moveTo 25 230
        lineTo 45 250
        lineTo 35 270
        lineTo 15 270
        lineTo 5 250
        closePath
        fill


    -- Fabrica
    drawFloorPlan :: Render ()
    drawFloorPlan = do
        -- Dibuja el plano de la fábrica
        setSourceRGB 0.0 0.0 0.0
        setLineWidth 2.0
        RendCairo.rectangle 70 10 320 240
        stroke

        -- Almacén
        RendCairo.rectangle 250 160 100 50
        stroke
        moveTo 270 190
        showText "Almacén"

        -- Zonas de recarga
        setSourceRGB 0.0 1.0 1.0
        RendCairo.rectangle 80 200 40 30
        stroke
        moveTo 93 220
        showText "R1"

        RendCairo.rectangle 130 200 40 30
        stroke
        moveTo 143 220
        showText "R2"

        RendCairo.rectangle 180 200 40 30
        stroke
        moveTo 193 220
        showText "R3"


    -- Dibuja los obstáculos (máquinas)
    drawObstacles :: [Obstacle] -> Render ()
    drawObstacles obstacles = do
        forM_ obstacles $ \(x, y, w, h, s) -> do
            setSourceRGB 0.5 0.5 0.5
            setLineWidth 2.0
            RendCairo.rectangle x y w h
            stroke
            
            setSourceRGB 0.0 0.0 0.0
            moveTo (x + 15) (y + 20)
            showText s



    drawAgentAreaHandler :: Adjustment -> DrawingArea -> [Agente] -> [Obstacle] -> Render ()
    drawAgentAreaHandler _ drawingArea agents obstacles = do
        drawShapes
        
        drawFloorPlan

        drawObstacles obstacles

        forM_ agents $ \agent -> do
            drawAgent agent
        


    valueChangedHandler :: DrawingArea -> IO ()
    valueChangedHandler paintArea = do
        widgetQueueDraw paintArea     