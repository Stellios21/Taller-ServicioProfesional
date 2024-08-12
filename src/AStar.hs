module AStar where

    import Graphics.UI.Gtk
    import Control.Monad.IO.Class      -- for liftIO
    import Graphics.Rendering.Cairo
    import Graphics.Rendering.Cairo as RendCairo
    import Control.Monad                --  for  ForM_
    import Data.Text (pack)

    import Graphics.UI.Gtk.Gdk.Screen
    import Graphics.UI.Gtk.Gdk.Pixbuf

    import Graphics.UI.Gtk.General.CssProvider (cssProviderLoadFromPath, cssProviderNew)
    import Graphics.UI.Gtk.Gdk.Screen (screenGetDefault, screenGetRootWindow)
    import Graphics.UI.Gtk.Abstract.Widget (widgetGetStyleContext)
    import Graphics.UI.Gtk.Gdk.GLContext
    import Graphics.UI.Gtk.Types

    import GI.Gtk.Objects.StyleContext 
    import GI.Gtk.Constants
    import Data.Word (Word32)

    import Data.IORef
    import Data.List (minimumBy)
    import Data.Ord (comparing)
    import qualified Data.Set as Set
    import qualified Data.Map as Map
    import Data.Maybe (fromMaybe)
    import Control.Concurrent (forkIO, threadDelay)
    import Graphics.UI.Gtk.Gdk.Events (eventButton)

    import qualified GI.Gtk as Gtk
    import qualified GI.Gdk as Gdk


    type Position = (Double, Double)
    type Obstacle = (Double, Double, Double, Double, String) -- (x, y, width, height)


    -- Verifica si el agente colisiona con algún obstáculo
    isCollision :: Position -> Obstacle -> Bool
    isCollision (x, y) (ox, oy, ow, oh, os) = x > ox && x < ox + ow && y > oy && y < oy + oh

    -- Movimientos posibles del agente (incluyendo diagonales)
    possibleMoves :: [Position]
    possibleMoves = [(-10, 0), (10, 0), (0, -10), (0, 10), (-10, -10), (10, 10), (-10, 10), (10, -10)]

    -- Función heurística (distancia Euclidiana)
    heuristic :: Position -> Position -> Double
    heuristic (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

    -- Algoritmo A* para encontrar la ruta óptima
    aStar :: Position -> Position -> [Obstacle] -> Maybe [Position]
    aStar start goal obstacles = aStar' Set.empty (Set.singleton start) (Map.singleton start 0) (Map.singleton start (heuristic start goal)) Map.empty
        where
            aStar' :: Set.Set Position -> Set.Set Position -> Map.Map Position Double -> Map.Map Position Double -> Map.Map Position Position -> Maybe [Position]
            aStar' closed open gScore fScore cameFrom
                | Set.null open = Nothing
                | current == goal = Just (reconstructPath cameFrom current)
                | otherwise = aStar' closed' open'' gScore' fScore' cameFrom'
                where
                    current = snd $ minimumBy (comparing fst) [(fScore Map.! pos, pos) | pos <- Set.toList open]
                    neighbors = filter (not . (`Set.member` closed)) $
                                filter (not . flip any obstacles . isCollision) $
                                map (\(dx, dy) -> (fst current + dx, snd current + dy)) possibleMoves
                    closed' = Set.insert current closed
                    (open'', gScore', fScore', cameFrom') = foldr update (Set.delete current open, gScore, fScore, cameFrom) neighbors

                    update neighbor (open, gScore, fScore, cameFrom)
                        | tentativeGScore < Map.findWithDefault (1 / 0) neighbor gScore =
                            (Set.insert neighbor open, Map.insert neighbor tentativeGScore gScore, Map.insert neighbor tentativeFScore fScore, Map.insert neighbor current cameFrom)
                        | otherwise = (open, gScore, fScore, cameFrom)
                        where
                            tentativeGScore = gScore Map.! current + heuristic current neighbor
                            tentativeFScore = tentativeGScore + heuristic neighbor goal

            reconstructPath cameFrom current = 
                case Map.lookup current cameFrom of
                    Nothing -> [current]
                    Just prev -> reconstructPath cameFrom prev ++ [current]