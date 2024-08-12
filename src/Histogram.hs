module Histogram where

    import Graphics.UI.Gtk
    import Graphics.Rendering.Cairo
    import Graphics.Rendering.Cairo as RendCairo
    import Control.Monad.IO.Class (liftIO)
    import Control.Monad
    import Data.IORef
    import System.Random (randomRIO)


    -- Zip con 4 listas
    zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
    zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a, b, c, d) : zip4 as bs cs ds
    zip4 _ _ _ _ = []


    drawHistogramChart :: [(Int, Int, Int, Int)] -> Render ()
    drawHistogramChart values = do
        let barWidth = 50
            spacing = 45
            maxHeight = 200
            labels = ["PARENT", "ADULT", "CHILD"]

        let combinedValues = zip4 (zip [0..] (map (\(a, _, _, _) -> a) values))
                                  (zip [0..] (map (\(_, b, _, _) -> b) values))
                                  (zip [0..] (map (\(_, _, c, _) -> c) values))
                                  (zip [0..] (map (\(_, _, _, d) -> d) values))

        mapM_ ( \(v1, v2, v3, v4) -> drawHistogram barWidth spacing maxHeight labels v1 v2 v3 v4) combinedValues


    drawHistogram :: Int -> Int -> Int -> [String] -> (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Render ()
    drawHistogram barWidth spacing maxHeight labels (i1, v1) (i2, v2) (i3, v3) (i4, v4) = do
        let label = labels !! i1  -- Seleccionar la etiqueta correspondiente
        
            x = fromIntegral (i1 * (barWidth + spacing) + 50)
            
            scaleY = fromIntegral maxHeight / 300

            y1 = fromIntegral maxHeight - fromIntegral v1 * scaleY
            y2 = y1 - fromIntegral v2 * scaleY
            y3 = y2 - fromIntegral v3 * scaleY
            y4 = y3 - fromIntegral v4 * scaleY
            
            height1 = fromIntegral v1 * scaleY
            height2 = fromIntegral v2 * scaleY
            height3 = fromIntegral v3 * scaleY
            height4 = fromIntegral v4 * scaleY

        -- Dibujar barras
            -- Rojo
        setSourceRGB 1 0 0
        RendCairo.rectangle x (y1 + 25) (fromIntegral barWidth) height1
        fill
        
            -- Naranja
        setSourceRGB 1 0.5 0 
        RendCairo.rectangle x (y2 + 25) (fromIntegral barWidth) height2
        fill
        
            -- Amarillo
        setSourceRGB 1 1 0 
        RendCairo.rectangle x (y3 + 25) (fromIntegral barWidth) height3
        fill
        
            -- Blanco
        setSourceRGB 1 1 1 
        RendCairo.rectangle x (y4 + 25) (fromIntegral barWidth) height4
        fill

        -- Dibujar borde de la barra
        setSourceRGB 0 0 0
        RendCairo.rectangle x 25 (fromIntegral barWidth) 200
        stroke

        -- Dibujar etiquetas del eje y
        forM_ [0, 25 .. 300] $ \v -> do
            let y = fromIntegral maxHeight - fromIntegral v * scaleY
            when (v `mod` 25 == 0) $ do
                setSourceRGB 0 0 0
                moveTo (x - 30) (y + 25)
                showText (show v)
                moveTo (x - 10) (y + 25)
                lineTo (x) (y + 25)
                stroke

        -- Dibujar etiquetas
        setSourceRGB 0 0 0
        moveTo (x + fromIntegral barWidth / 4) 15
        showText label


    drawHistogramHandler :: Adjustment -> DrawingArea -> IORef [(Int, Int, Int, Int)] -> Render ()
    drawHistogramHandler _ drawingArea valuesRef = do
        values <- liftIO $ readIORef valuesRef
        drawHistogramChart values


    -- Generar una tupla de cuatro valores aleatorios
    randomTuple :: IO (Int, Int, Int, Int)
    randomTuple = do
        v1 <- randomRIO (0, 300)
        v2 <- randomRIO (0, (300 - v1))
        v3 <- randomRIO (0, (300 - v1 - v2))
        v4 <- randomRIO (0, (300 - v1 - v2 - v3))
        return (v1, v2, v3, v4)


    updateHistogramChart :: DrawingArea -> IORef [(Int, Int, Int, Int)] -> IO Bool
    updateHistogramChart drawingArea valuesRef = do
        values <- readIORef valuesRef
            
        newValues <- mapM (const randomTuple) [1..3]
        writeIORef valuesRef newValues
        
        widgetQueueDraw drawingArea
        return True
