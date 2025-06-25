import Graphics.UI.Gtk

main :: IO ()
main = do
    _ <- initGUI

    -- Crea una ventana nueva
    window <- windowNew
    set window [windowTitle := "Ejemplo de Alignment", containerBorderWidth := 10, windowDefaultWidth := 300, windowDefaultHeight := 200]
    on window objectDestroy mainQuit

    -- Crea una caja vertical (VBox)
    vbox <- vBoxNew False 0
    containerAdd window vbox

    -- Crea un botón
    button <- buttonNewWithLabel "Botón"
    button2 <- buttonNewWithLabel "Botón 2"

    -- Crea un widget Alignment y establece sus propiedades
    alignment <- alignmentNew 0.5 0.5 0.1 0.1 -- xalign, yalign, xscale, yscale
    containerAdd alignment button

    alignment2 <- alignmentNew 0.5 0.5 0.1 0.1 -- xalign, yalign, xscale, yscale
    containerAdd alignment2 button2

    -- Añade el widget Alignment a la caja
    boxPackStart vbox alignment PackGrow 0
    boxPackStart vbox alignment2 PackGrow 0

    -- Muestra todos los widgets
    widgetShowAll window
    mainGUI
