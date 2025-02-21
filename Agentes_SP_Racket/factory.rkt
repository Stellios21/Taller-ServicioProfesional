#lang racket/gui

(require racket/gui/base)
(require "agent.rkt")
(require "rechargeZone.rkt")
(require "machine.rkt")

;; Crear agentes de prueba
;(define agente1 (new agent% [id 1] [is-red #t] [energia-inicial 300] [posicion-inicial '(50 50)] [area-vision 100]))
(define agente2 (new agent% [id 2] [is-red #f] [energia-inicial 150] [posicion-inicial '(190 150)] [area-vision 100]))
;(define agentes (list agente1 agente2))
(define agentes (list agente2))

;; Crear zonas de recarga
(define rechargeZones
  (list
   (new rechargeZone% [id "R1"] [posicion '(20 230 30 30)])
   (new rechargeZone% [id "R2"] [posicion '(60 230 30 30)])
   (new rechargeZone% [id "R3"] [posicion '(100 230 30 30)])))

;; Crear máquinas
(define machines
  (list
   (new machine% [id "M1"] [posicion '(100 40 50 30)])
   (new machine% [id "M2"] [posicion '(100 100 50 30)])
   (new machine% [id "M3"] [posicion '(180 40 50 30)])))

(define objects
  (append rechargeZones machines))


;; Función para dibujar el diseño de la fábrica y los objetos
(define (dibujar-layout canvas dc agentes rechargeZones machines)
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

;; Frame principal
(define frame (new frame% [label "Fábrica"] [width 400] [height 300]))

;; Canvas para el dibujo
(define factory-canvas
  (new canvas%
       [parent frame]
       [min-width 400]
       [min-height 300]
       [paint-callback
        (λ (canvas dc)
          (dibujar-layout canvas dc agentes rechargeZones machines))]))

;; Temporizador para mover los agentes
(define temporizador
  (new timer%
       [notify-callback
        (λ ()
          (if (ormap (λ (agente) (> (send agente estado-energia) 0)) agentes)
              (begin
                (for-each (λ (agente) (send agente bucle-principal (send factory-canvas get-dc) objects)) agentes)
                (send factory-canvas refresh)
                (for-each (λ (agente) (send agente reducir-energia)) agentes)
                (printf "\n"))
              (begin
                (send temporizador stop)
                (printf "Todos los agentes han agotado su energía. Deteniendo ciclo.\n"))))]))



;; Botón para iniciar movimiento
(define iniciar-boton
  (new button%
       [parent frame]
       [label "Iniciar Movimiento"]
       [callback
        (λ (btn evt)
          ;; Inicia el temporizador para actualizar los agentes
          (send temporizador start 100))]))

;; Mostrar la ventana principal
(send frame show #t)
