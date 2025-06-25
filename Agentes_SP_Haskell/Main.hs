import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Función para manejar eventos (en este caso no se usa)
handleEvent :: Event -> Float -> Float
handleEvent _ = id

-- Función para actualizar el estado
update :: Float -> Float -> Float
update seconds x = x + 100 * seconds

-- Estado inicial (posición del círculo)
initialState :: Float
initialState = 0

-- Función para dibujar el estado actual
render :: Float -> Picture
render _ = Pictures [ place, storage, 
                      rechargePlace1, rechargePlace2, rechargePlace3, 
                      machine1, machine2, machine3, 
                      materials, agents
                    ]

-- Contorno de la estructura
place :: Picture
place = Pictures 
        [ Translate 0 0 $ Color black $ Line [(-300, 200), (300, 200), (300, -200), (-300, -200), (-300, 200)]
        , Translate (-200) 200 $ Color white $ Line [(-50, 0), (30, 0)]
        ]

-- Zonas de carga
rechargePlace1 :: Picture
rechargePlace1 = Pictures
    [ Translate (-265) (-140) $ Color (makeColorI 64 224 208 255) $ rectangleWire 50 100
    , Translate (-275) (-140) $ Scale 0.15 0.15 $ Color black $ Text "R1"
    ]

rechargePlace2 :: Picture
rechargePlace2 = Pictures
    [ Translate (-205) (-140) $ Color (makeColorI 64 224 208 255) $ rectangleWire 50 100
    , Translate (-215) (-140) $ Scale 0.15 0.15 $ Color black $ Text "R2"
    ]

rechargePlace3 :: Picture
rechargePlace3 = Pictures
    [ Translate (-145) (-140) $ Color (makeColorI 64 224 208 255) $ rectangleWire 50 100
    , Translate (-155) (-140) $ Scale 0.15 0.15 $ Color black $ Text "R3"
    ]

-- Almacen
storage :: Picture
storage = Pictures
    [ Translate (145) (-140) $ Color black $ rectangleWire 295 100
    , Translate (105) (-140) $ Scale 0.15 0.15 $ Color black $ Text "Almacen"
    , Translate (145) (-90) $ Color white $ Line [(-50, 0), (30, 0)]
    ]

-- Maquinas
machine1 :: Picture
machine1 = Pictures
    [ Translate (145) (140) $ Color black $ rectangleWire 100 50
    , Translate (135) (135) $ Scale 0.15 0.15 $ Color black $ Text "M1"
    ]

machine2 :: Picture
machine2 = Pictures
    [ Translate (0) (140) $ Color black $ rectangleWire 100 50
    , Translate (-15) (135) $ Scale 0.15 0.15 $ Color black $ Text "M2"
    ]

machine3 :: Picture
machine3 = Pictures
    [ Translate (0) (-20) $ Color black $ rectangleWire 100 50
    , Translate (-15) (-25) $ Scale 0.15 0.15 $ Color black $ Text "M3"
    ]

-- Materiales
materials :: Picture
materials = Pictures
    [ Translate (-350) 150 $ Color yellow $ rectangleSolid 30 30     -- Cuadrado amarillo
    , Translate (-350) 100 $ Color green $ rotate 45 $ rectangleSolid 30 30   -- Rombo verde
    , Translate (-350) 50 $ Color orange $ circleSolid 15           -- Circulo naranja
    , Translate (-350) 0 $ Color blue $ polygon (polygonPoints 5 20)   -- Pentágono azul
    , Translate (-350) (-50) $ Color (makeColorI 128 0 128 255) $ polygon (polygonPoints 6 20)  -- Hexágono morado
    ]

-- Agentes
agents :: Picture
agents = Pictures
    [ Translate 0 (120) $ Color red $ agentShape    
    , Translate 50 (120) $ Color blue $ agentShape  
    ]

-- Figura de los agentes
agentShape :: Picture
agentShape = Pictures[Translate 0 (-50) $ circleSolid 10]

-- Generar puntos para un polígono regular
polygonPoints :: Int -> Float -> [Point]
polygonPoints sides radius = [ (radius * cos (2 * pi * fromIntegral k / fromIntegral sides), radius * sin (2 * pi * fromIntegral k / fromIntegral sides)) | k <- [1..sides] ]




-- Función principal para configurar la ventana y ejecutar el programa
main :: IO ()
main = play
    (InWindow "Taller" (1280, 700) (100, 100)) -- Ventana
    white           -- Color de fondo
    60              -- FPS (frames per second)
    initialState    -- Estado inicial
    render          -- Función de renderizado
    (\_ s -> s)     -- Función de manejo de eventos (no se usa)
    (\_ s -> s)     -- Función de actualización (no se usa)
