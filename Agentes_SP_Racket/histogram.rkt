#lang racket/gui

(require racket/draw) ; Necesario para usar color%
(require racket/gui)  ; Necesario para usar timer%

(provide draw-histograms)

(define width 500)  ; Ancho total del área de los histogramas
(define height 300) ; Altura total para los marcadores
(define scaled-height 200) ; Altura máxima para los histogramas
(define bar-width (/ width 10)) ; Ancho de cada barra
(define margin 50) ; Margen izquierdo para los marcadores
(define text-size 12) ; Tamaño de fuente para los números de los marcadores
(define offset-y 30) ; Desplazamiento hacia abajo de todos los elementos

; Factor de escala para ajustar las alturas de los histogramas
(define scale-factor (/ scaled-height height))

; Generar rangos aleatorios que respeten las reglas
(define (generate-ranges)
  (let* ([red-start 0]
         [red-end (+ red-start (random 1 100))]
         [orange-start red-end]
         [orange-end (+ orange-start (random 1 100))]
         [yellow-start orange-end]
         [yellow-end (+ yellow-start (random 1 100))]
         [white-start yellow-end]
         [white-end height]) ; Mantener el rango máximo en 300
    (list (list red-start red-end (make-object color% 255 0 0))    ; Rojo
          (list orange-start orange-end (make-object color% 255 165 0)) ; Naranja
          (list yellow-start yellow-end (make-object color% 255 255 0)) ; Amarillo
          (list white-start white-end (make-object color% 255 255 255))))) ; Blanco


; Dibujar un histograma de abajo hacia arriba
(define (draw-histogram dc x-offset ranges)
  (send dc draw-rectangle x-offset (- offset-y 1) bar-width 200) ; Dibuja un contorno de la barra
  (define current-y (+ scaled-height offset-y)) ; Comienza desde la base del canvas escalado
  
  (for ([range ranges])
    (let* ([start (* scale-factor (first range))]  ; Calcula la altura de inicio
           [end (* scale-factor (second range))]  ; Calcula la altura de fin
           [color (third range)]                  ; Obtiene el color
           [rect-height (- end start)]            ; Calcula la altura del rectángulo
           [adjusted-height (min rect-height scaled-height)]) ; Ajusta para que no supere el área máxima
      (when (and (>= rect-height 0) (>= adjusted-height 1)) ; Verifica que no haya valores negativos
        ; Dibuja el rectángulo con el color especificado
        (send dc set-brush (make-object brush% color 'solid))
        (send dc draw-rectangle x-offset (- current-y adjusted-height) bar-width (- adjusted-height 1))
        
        ; Dibuja un rectángulo transparente encima para mantener el contorno
        (send dc set-brush (make-object brush% "white" 'transparent))
        (send dc draw-rectangle x-offset (- current-y adjusted-height) bar-width adjusted-height)
        
        (set! current-y (- current-y adjusted-height)))))) ; Ajustar la posición para el siguiente rectángulo


; Dibujar los marcadores y líneas horizontales
(define (draw-markers dc x-offset)
  (send dc set-font (make-object font% 10 'default 'normal 'normal #f 'default #f)) ; Establecer tamaño de texto
  (for ([y (in-range 0 (+ 1 height) 25)]) ; Cada 25 unidades
    (let ([real-y (- (+ scaled-height offset-y) (* scale-factor y))]) ; Ajustar al sistema de coordenadas escalado
      (send dc draw-line x-offset (- real-y 1) (+ x-offset bar-width) (- real-y 1)) ; Línea horizontal
      (send dc set-text-foreground "black")
      (send dc draw-text (number->string y) (- x-offset 30) (- real-y 10))))) ; Etiquetas de valores

; Dibujar histogramas en un canvas con etiquetas
(define (draw-histograms-on-canvas canvas)
  (define dc (send canvas get-dc))
  (send dc clear)
  (send dc set-pen "black" 2 'solid)
  (let ([ranges1 (generate-ranges)]
        [ranges2 (generate-ranges)]
        [ranges3 (generate-ranges)])
    ; Dibujar el primer histograma con sus marcadores
    (draw-markers dc margin)
    (draw-histogram dc (+ margin 10) ranges1)
    (send dc set-text-foreground "black")
    (send dc set-font (make-object font% 10 'default 'normal 'normal #f 'default #f))
    (send dc draw-text "PARENT" margin (- offset-y 30)) ; Etiqueta encima del primer histograma
    
    ; Dibujar el segundo histograma con sus marcadores
    (draw-markers dc (+ margin (* 2 bar-width) 10))
    (draw-histogram dc (+ margin (* 2 bar-width) 20) ranges2)
    (send dc set-font (make-object font% 10 'default 'normal 'normal #f 'default #f))
    (send dc draw-text "ADULT" (+ margin (* 2 bar-width) 10) (- offset-y 30)) ; Etiqueta encima del segundo histograma
    
    ; Dibujar el tercer histograma con sus marcadores
    (draw-markers dc (+ margin (* 4 bar-width) 20))
    (draw-histogram dc (+ margin (* 4 bar-width) 30) ranges3)
    (send dc set-font (make-object font% 10 'default 'normal 'normal #f 'default #f))
    (send dc draw-text "CHILD" (+ margin (* 4 bar-width) 20) (- offset-y 30)))) ; Etiqueta encima del tercer histograma

; Dibujar histogramas con actualización periódica
(define (draw-histograms panel)
  (define canvas
    (new canvas%
         [parent panel]
         [min-width 340]
         [min-height 250]
         [paint-callback
          (lambda (canvas dc)
            (draw-histograms-on-canvas canvas))]))
  ; Temporizador para actualizar el canvas cada 1 segundo
  (new timer%
       [notify-callback
        (lambda ()
          (send canvas refresh))] ; Redibuja el canvas
       [interval 1000])) ; Intervalo en milisegundos