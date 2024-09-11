module Agent where

    import Graphics.UI.Gtk
    import Control.Monad.IO.Class 
    import Graphics.Rendering.Cairo
    import Graphics.Rendering.Cairo as RendCairo
    import Control.Monad 
    import Data.Text (pack)

    import Control.Monad.IO.Class (liftIO)
    import Control.Concurrent (forkIO, threadDelay)

    import qualified GI.Gtk as Gtk
    import qualified GI.Gdk as Gdk

    import Data.IORef

    import AStar


    data Agente = Agente
        { 
          idAgente :: Int
        , isRed    :: Bool       
        , vida     :: Int       
        , posicion :: IORef (Position)
        }


    class AgenteClass a where
        estaVivo :: a -> Bool
        decrementarVida :: a -> a
        drawAgent :: a -> Render ()
        moverAgente :: Adjustment -> DrawingArea -> a -> Position -> [Obstacle] -> IO ()
        comer :: a -> IO a
        
    --comunicarse

    instance AgenteClass Agente where


        estaVivo agente = vida agente > 0


        decrementarVida agente = agente { vida = vida agente - 1 }

        
        drawAgent agente = do
            (x, y) <- liftIO $ readIORef (posicion agente)
            if (isRed agente)
            then do
                setSourceRGB 1.0 0.0 0.0 -- Rojo
                arc x y 10 0 (2 * pi)
                fill
            else do
                setSourceRGB 0.0 0.0 1.0 -- Azul
                arc x y 10 0 (2 * pi)
                fill


        moverAgente _ drawArea agente targetPos obstacles = do 
            currentPos <- liftIO $ readIORef (posicion agente) 
            case aStar currentPos targetPos obstacles of
                Just path -> do
                    let updatePosition (x, y) (tx, ty) = 
                            let dx = if x < tx then 1 else if x > tx then -1 else 0
                                dy = if y < ty then 1 else if y > ty then -1 else 0
                            in (x + dx, y + dy)
                    
                    let moveAgent [] = return ()
                        moveAgent (pos:ps) = do
                            writeIORef (posicion agente) pos
                            postGUIAsync $ widgetQueueDraw drawArea
                            threadDelay 50000
                            moveAgent ps
                    liftIO $ forkIO (moveAgent path) >> return ()
        
        comer agente = do
            (x, y) <- liftIO $ readIORef (posicion agente)
            if isCollision (x, y) (80, 200, 40, 30, "R1") ||
               isCollision (x, y) (130, 200, 40, 30, "R2") ||
               isCollision (x, y) (180, 200, 40, 30, "R3")
            then return agente { vida = vida agente + 1 }
            else return agente

                


    -- Filtra los agentes vivos de una lista
    filtrarAgentesVivos :: [Agente] -> [Agente]
    filtrarAgentesVivos = filter estaVivo


