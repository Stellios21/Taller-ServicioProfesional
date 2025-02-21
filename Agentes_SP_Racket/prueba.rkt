#lang racket/gui

(require "factory.rkt")

; Crear la ventana principal
(define frame (new frame%
                   [label "Histogramas en Panel"]
                   [width 600]
                   [height 350]))

; Crear un panel para los histogramas
(define panel (new panel% [parent frame]))

; Dibujar los histogramas en el panel
(draw-factory-layout panel
                     '((220 100 "red") (180 100 "blue")) ; agentes
                     '((100 40 50 30 "M1") (100 100 50 30 "M2") (180 40 50 30 "M3")) ; máquinas
                     '((20 230 30 30 "R1") (60 230 30 30 "R2") (100 230 30 30 "R3")) ; zonas de recarga
)

; Mostrar la ventana
(send frame show #t)
