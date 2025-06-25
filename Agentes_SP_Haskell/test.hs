import Graphics.UI.Gtk
import Control.Monad.IO.Class      -- for liftIO
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo as RendCairo
import Control.Monad                --  for  ForM_
import Data.Text (pack)



import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk


main :: IO ()
main = do
  initGUI
  window <- windowNew
  adjustment <- adjustmentNew 50.0 0.0 101.0 1.0 1.0 1.0
  
  mainBox <- hBoxNew False 2

  set window [windowDefaultWidth := 1280,
              windowDefaultHeight := 700,
              windowWindowPosition := WinPosCenter,
              containerChild := mainBox,
              windowTitle := "Taller"]
  windowSetPosition window WinPosCenter 
  on window objectDestroy mainQuit 

  vbox <- vBoxNew False 2

  -- Crear algunos widgets
  label <- labelNew (Just "Hello, World!")
  button <- buttonNewWithLabel "Click Me"

  -- Ajustar el tamaño de los widgets
  widgetSetSizeRequest label 100 30
  widgetSetSizeRequest button 100 30
  
  boxPackStart vbox label PackNatural 0
  boxPackStart vbox button PackNatural 0

  boxPackStart mainBox vbox PackGrow 0

  -- Cargar y aplicar el archivo CSS
  cssProvider <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromPath cssProvider (pack "C:\\Users\\ASUS\\Documents\\UABC\\Servicio Profesional\\Agentes_SP\\src\\style.css")
  screen <- Gdk.screenGetDefault
  case screen of
      Just scr -> Gtk.styleContextAddProviderForScreen scr cssProvider (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)
      Nothing  -> putStrLn "No se pudo obtener la pantalla"

  widgetShowAll window
  mainGUI
