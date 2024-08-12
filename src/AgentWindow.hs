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


    type Position = (Double, Double)
    type Obstacle = (Double, Double, Double, Double, String) -- (x, y, width, height)

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

        -- Agentes
        setSourceRGB 1.0 0.0 0.0
        arc 210 80 10 0 (2 * pi)
        fill

        setSourceRGB 0.0 0.0 1.0
        arc 240 80 10 0 (2 * pi)
        fill

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


    -- Dibuja los obstáculos (máquinas) ##
    drawObstacles :: [Obstacle] -> Render ()
    drawObstacles obstacles = do
        forM_ obstacles $ \(x, y, w, h, s) -> do
            setSourceRGB 0.5 0.5 0.5
            setLineWidth 2.0
            RendCairo.rectangle x y w h
            stroke
            
            setSourceRGB 0.0 0.0 0.0
            moveTo (x+15) (y+20)
            showText s


    -- Dibuja el agente en su posición actual
    drawAgent :: Position -> Render ()
    drawAgent (x, y) = do
        setSourceRGB 1.0 0.0 0.0 -- Rojo
        arc x y 10 0 (2 * pi)
        fill

        --setSourceRGB 0.0 0.0 1.0
        --arc 240 80 10 0 (2 * pi)
        --fill

    -- Dibuja la escena completa (obstáculos y agente)
    drawScene :: IORef Position -> [Obstacle] -> Render ()
    drawScene agentPosRef obstacles = do
        drawObstacles obstacles
        pos <- liftIO $ readIORef agentPosRef
        drawAgent pos


    drawAgentAreaHandler :: Adjustment -> DrawingArea -> IORef Position -> [Obstacle] -> Render ()
    drawAgentAreaHandler _ drawingArea agentPosRef obstacles = do
        -- Dibuja los shapes
        drawShapes
        -- Dibuja el plano
        drawFloorPlan

        drawScene agentPosRef obstacles


    valueChangedHandler :: DrawingArea -> IO ()
    valueChangedHandler paintArea = do
        widgetQueueDraw paintArea     