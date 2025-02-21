#lang racket/gui

(provide rechargeZone%)

;; Definimos la clase rechargeZone
(define rechargeZone%
  (class object%
    (init-field id posicion) ; id: Identificador, posicion: (x, y)

    ;; Campo constante para el tipo
    (define tipo "Zona de Recarga")

    ;; Método para dibujar la zona
    (define/public (dibujar dc)
      (let ([x (first posicion)]
            [y (second posicion)]
            [w (third posicion)]
            [h (fourth posicion)])
        (send dc set-pen (make-object pen% "cyan" 2 'solid))
        (send dc set-brush (make-object brush% "white" 'solid))
        (send dc draw-rectangle x y w h) ; Dibujar el rectángulo
        (send dc draw-text id (+ x 5) (+ y 5)))) ; Dibujar el texto del ID

    ;; Método para obtener la posición
    (define/public (obtener-posicion)
      posicion)

    ;; Método para obtener el ID
    (define/public (obtener-id)
      id)

    ;; Método para obtener el tipo
    (define/public (obtener-tipo)
      tipo)

    ;; Finalizamos la definición de la clase
    (super-new)))
