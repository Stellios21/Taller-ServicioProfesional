#lang racket/gui

(provide draw-factory-items) ; Exportar la función
(provide draw-factory-canvas)

;; Función principal que genera el diseño de la fábrica
(define (draw-factory-items parent-panel)
  (define left-panel
    (new vertical-panel%
         [parent parent-panel]
         [spacing 0]))
  
  (define shapes-canvas
    (new canvas%
         [parent left-panel]
         [min-width 50]
         [min-height 280]
         [paint-callback
          (λ (canvas dc)
            (send dc set-pen (make-object pen% "black" 2 'solid))

            (send dc set-brush (make-object brush% "yellow" 'solid))
            (send dc draw-rectangle 10 10 30 30)

            (send dc set-brush (make-object brush% "green" 'solid))
            (send dc draw-polygon (list (cons 25 60) (cons 40 90) (cons 10 90)))

            (send dc set-brush (make-object brush% "orange" 'solid))
            (send dc draw-ellipse 10 115 35 35)

            (send dc set-brush (make-object brush% "blue" 'solid))
            (send dc draw-polygon (list (cons 25 170) (cons 45 190) (cons 35 210)
                                        (cons 15 210) (cons 5 190)))

            (send dc set-brush (make-object brush% "purple" 'solid))
            (send dc draw-polygon (list (cons 25 230) (cons 45 250) (cons 35 270)
                                        (cons 15 270) (cons 5 250))))]))

  ;; Retornar el panel principal como resultado
  left-panel)

  
;; Función para dibujar el diseño de la fábrica y los objetos
(define (draw-factory-canvas canvas dc agentes rechargeZones machines)
  ;; Dibujar el diseño de la fábrica
  (send dc set-pen (make-object pen% "black" 2 'solid))
  (send dc set-brush (make-object brush% "white" 'solid))
  (send dc draw-rectangle 10 10 340 260) ; Contorno principal

  ;; Almacén
  (send dc draw-rectangle 190 190 100 50)
  (send dc draw-text "Almacén" 210 205)

  ;; Dibujar máquinas
  (for-each (λ (machine) (send machine dibujar dc)) machines)

  ;; Dibujar zonas de recarga
  (for-each (λ (zone) (send zone dibujar dc)) rechargeZones)

  ;; Dibujar los agentes
  (send dc set-pen (make-object pen% "black" 1 'solid))
  (for-each
   (λ (agente) (send agente dibujar dc))
   agentes))
  


