module Graphic where

    import Graphics.UI.Gtk
    import Graphics.Rendering.Cairo
    import Graphics.Rendering.Cairo as RendCairo
    import Control.Monad.IO.Class (liftIO)
    import Control.Monad
    import Data.IORef
    import System.Random (randomRIO)


    drawGraphic :: Adjustment -> DrawingArea -> IORef [(Int, Int)] -> Render ()
    drawGraphic _ drawingArea valuesRef = do
        values <- liftIO $ readIORef valuesRef
        drawLineChart values
    
 
    drawLineChart :: [(Int, Int)] -> Render ()
    drawLineChart values = do
        -- Configurar tamaños
        let maxWidth = 450
            maxHeight = 125
            scaleX = fromIntegral maxWidth / fromIntegral (length values - 1)
            scaleY = fromIntegral maxHeight / 100 -- Suponiendo que el valor máximo es 100

        setSourceRGB 0 0 0
        RendCairo.rectangle 50 25 (fromIntegral maxWidth) (fromIntegral maxHeight)
        fill

        -- Dibujar ejes
        setSourceRGB 0 0 0 -- Negro
        moveTo 50 (fromIntegral maxHeight + 25)
        lineTo (fromIntegral maxWidth + 50) (fromIntegral maxHeight + 25)
        stroke
        
        moveTo 50 25 --Y
        lineTo 50 (fromIntegral maxHeight + 25)
        stroke

        -- Dibujar etiquetas del eje y
        forM_ [0, 5 .. 100] $ \v -> do
            let y = fromIntegral maxHeight - fromIntegral v * scaleY
            if v `mod` 20 == 0
            then do
                setSourceRGB 0 0 0
                moveTo 30 (y+25)
                showText (show v)
            else return ()

            setSourceRGB 0.5 0.5 0.5
            moveTo 50 (y+25)
            lineTo (fromIntegral maxWidth + 50) (y + 25)
            stroke

        -- Dibujar etiquetas del eje x
        forM_ (zip values [0..]) $ \((t, _), i) -> do
            let x = fromIntegral i * scaleX
            moveTo (x+50) (fromIntegral maxHeight + 40)
            setSourceRGB 0 0 0
            if t `mod` 5 == 0
            then showText (show t)
            else do 
                lineTo (x+50) (fromIntegral maxHeight + 25)
                stroke

            moveTo (x+50) (fromIntegral maxHeight + 25)
            setSourceRGB 0.5 0.5 0.5 
            lineTo (x+50) 25
            stroke
        
        setSourceRGB 0 0 0
        moveTo ((fromIntegral maxWidth + 60)/2) (fromIntegral maxHeight + 60)
        showText ("Time")

        -- Dibujar línea
        setSourceRGB 0 1 0 -- Azul
        setLineWidth 2.0
        forM_ (zip values (tail values)) $ \((t1, v1), (t2, v2)) -> do
            let x1 = fromIntegral t1 * scaleX
                y1 = fromIntegral maxHeight - fromIntegral v1 * scaleY
                x2 = fromIntegral t2 * scaleX
                y2 = fromIntegral maxHeight - fromIntegral v2 * scaleY
            moveTo (x1+50) (y1+25)
            lineTo (x2+50) (y2+25)
            stroke


    updateChart :: DrawingArea -> IORef [(Int, Int)] -> IO Bool
    updateChart drawingArea valuesRef = do
        -- Leer los valores actuales
        values <- readIORef valuesRef

        -- Generar un nuevo valor aleatorio
        let (lastTime, _) = last values
        newValue <- (\rand -> (lastTime + 1, rand)) <$> randomRIO (0, 100)
        let newValues = values ++ [newValue] -- Mantener sólo los últimos 5 valores
        
        -- Actualizar los valores en la referencia IO
        writeIORef valuesRef newValues

        -- Solicitar redibujar el área de dibujo
        widgetQueueDraw drawingArea

        -- Continuar con el temporizador
        return True