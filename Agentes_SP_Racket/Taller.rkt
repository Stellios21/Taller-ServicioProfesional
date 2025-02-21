#lang racket/gui

(require racket/gui/base)

(require "factoryLayout.rkt")
(require "histogram.rkt")
(require "agent.rkt")
(require "rechargeZone.rkt")
(require "machine.rkt")

(define frame (new frame% [label "Taller"] [width 1320] [height 520]))



; Panel principal
(define main-panel
  (new vertical-panel%
       [parent frame]))


;; Panel izquiero principal
(define left-main-panel
  (new vertical-panel%
       [parent main-panel]
       [stretchable-height #f]
       [alignment '(left top)]
       [spacing 10]))


;; Panel superior
(define top-panel
  (new horizontal-panel%
       [parent left-main-panel]
       [stretchable-height #f]))

;;; Panel de control
(define control-panel
  (new horizontal-panel%
       [parent top-panel]
       [alignment '(left top)]
       [stretchable-width #f]
       [stretchable-height #f]
       [spacing 5]))


;; Botones de control
(define start-panel
  (new vertical-panel%
       [parent control-panel]))

(define start-button
  (new button%
       [parent start-panel]
       [label "Start"]
       [callback
        (λ (btn evt)
          ;; Inicia el temporizador para actualizar los agentes
          (send temporizador start 100))]))



(new button% [parent start-panel]
             [label "Stop"]
             [callback (λ (btn event) (displayln "Stop pressed"))])


;; Campos de tiempo
(define parameters-panel
  (new vertical-panel%
       [parent control-panel]
       [stretchable-width #t]
       [min-width 125]))

(define time-field
  (new text-field%
       [parent parameters-panel]
       [label "TIME "]
       [init-value "0"]
       [enabled #f]))

(define time-limit-field
  (new text-field%
       [parent parameters-panel]
       [label "TIME LIMIT "]
       [init-value "0"]))

(define p2s-field
  (new text-field%
       [parent parameters-panel]
       [label "P2S "]
       [init-value "0"]
       [enabled #f]))

(define goal-field
  (new text-field%
       [parent parameters-panel]
       [label "GOAL "]
       [init-value "0"]
       [min-width 50]))


(define state-action-label-panel
  (new vertical-panel%
       [parent control-panel]
       [alignment '(center bottom)]))

(new message% [parent state-action-label-panel] [label "State-Action"])
(new message% [parent state-action-label-panel] [label "Graph"])


(define battery-panel
  (new vertical-panel%
       [parent control-panel]))

(new message% [parent battery-panel] [label "Battery"])

(define battery
  (new gauge%
         [parent battery-panel]
         [label ""]
         [range 100]
         [style '(vertical)]))
(send battery set-value 100)


(define performance-panel
  (new vertical-panel%
       [parent control-panel]))

(new message% [parent performance-panel] [label "Performance"])

(define performance
  (new gauge%
         [parent performance-panel]
         [label ""]
         [range 100]
         [style '(horizontal)]))
(send performance set-value 100)

(define rate-field
  (new text-field%
       [parent performance-panel]
       [label "Rate "]
       [init-value "0"]))

(define real-rate-field
  (new text-field%
       [parent performance-panel]
       [label "Real Rate "]
       [init-value "0"]
       [enabled #f]))

(define token-field
  (new text-field%
       [parent performance-panel]
       [label "Token "]
       [init-value "0"]
       [enabled #f]))




; Panel inferior del panel izquierdo
(define bottom-panel
  (new horizontal-panel%
       [parent left-main-panel]
       [stretchable-width #f]
       [stretchable-height #f]
       [spacing 10]
       [alignment '(left top)]))


;; Panel de contenido
(define content-panel
  (new horizontal-panel%
       [parent bottom-panel]
       [stretchable-height #f]))


;;; Sección izquierda
(define left-panel
  (new vertical-panel%
       [parent content-panel]
       [stretchable-height #f]
       [alignment '(left top)]))


;; Etiquetas de columnas
(define labels-columns-panel
  (new horizontal-panel%
       [parent left-panel]
       [stretchable-width #f]
       [alignment '(right center)]))

(new message% [parent labels-columns-panel] [label ""] [min-width 20])
(new message% [parent labels-columns-panel] [label "PROFILE INTEREST LEVELS"])
(new message% [parent labels-columns-panel] [label "  MOTIVATION"])

(define labels-interest-panel
  (new vertical-panel%
       [parent labels-columns-panel]
       [stretchable-width #f]
       [stretchable-height #f]
       [alignment '(center center)]))

(new message% [parent labels-interest-panel] [label "INTEREST LEVELS"])

(define sublabels-interest-panel
  (new horizontal-panel%
       [parent labels-interest-panel]
       [stretchable-width #t]
       [alignment '(center center)]))

(new message% [parent sublabels-interest-panel] [label "EATING"])
(new message% [parent sublabels-interest-panel] [label "WORKING"])
(new message% [parent sublabels-interest-panel] [label "RESTING"])


;; Paneles para cada seccion
(define parent-configuration
  (new horizontal-panel%
       [parent left-panel]
       [stretchable-width #f]
       [stretchable-height #f]
       [spacing 10]))

(define adult-configuration
  (new horizontal-panel%
       [parent left-panel]
       [stretchable-width #f]
       [stretchable-height #f]
       [spacing 10]))

(define child-configuration
  (new horizontal-panel%
       [parent left-panel]
       [stretchable-width #f]
       [stretchable-height #f]
       [spacing 10]))


;; Etiquetas en vertical
(define parent-label
  (new vertical-panel%
       [parent parent-configuration]
       [spacing 0]
       [alignment '(center center)]))

(new message% [parent parent-label] [label "P"])
(new message% [parent parent-label] [label "A"])
(new message% [parent parent-label] [label "R"])
(new message% [parent parent-label] [label "E"])
(new message% [parent parent-label] [label "N"])
(new message% [parent parent-label] [label "T"])


(define adult-label
  (new vertical-panel%
       [parent adult-configuration]
       [spacing 0]
       [alignment '(center center)]))

(new message% [parent adult-label] [label "A"])
(new message% [parent adult-label] [label "D"])
(new message% [parent adult-label] [label "U"])
(new message% [parent adult-label] [label "L"])
(new message% [parent adult-label] [label "T"])


(define child-label
  (new vertical-panel%
       [parent child-configuration]
       [spacing 0]
       [alignment '(center center)]))

(new message% [parent child-label] [label "C"])
(new message% [parent child-label] [label "H"])
(new message% [parent child-label] [label "I"])
(new message% [parent child-label] [label "L"])
(new message% [parent child-label] [label "D"])



;; Profile interest levels
;;    parent
(define profile-parent-panel
  (new vertical-panel%
       [parent parent-configuration]
       [stretchable-height #f]))

(define eat-one-profile-panel
  (new horizontal-panel%
       [parent profile-parent-panel]))

(new message%
     [parent eat-one-profile-panel]
     [label "EAT        "])

(let ([choice-widget
       (new choice% [parent eat-one-profile-panel] 
              [label ""] 
              [choices '("very low" "low" "medium" "high" "very high")])])
    ;; Seleccionar el valor inicial como "medium"
    (send choice-widget set-selection 2))

(new message%
     [parent eat-one-profile-panel]
     [label "          "])

(new message%
     [parent eat-one-profile-panel]
     [label "0"]
     [min-width 32])


(define work-one-profile-panel
  (new horizontal-panel%
       [parent profile-parent-panel]))

(new message%
     [parent work-one-profile-panel]
     [label "WORK   "])

(let ([choice-widget
       (new choice% [parent work-one-profile-panel] 
              [label ""] 
              [choices '("very low" "low" "medium" "high" "very high")])])
    ;; Seleccionar el valor inicial como "medium"
    (send choice-widget set-selection 2))

(new message%
     [parent work-one-profile-panel]
     [label "          "])

(new message%
     [parent work-one-profile-panel]
     [label "0"]
     [min-width 32])


(define rest-one-profile-panel
  (new horizontal-panel%
       [parent profile-parent-panel]))

(new message%
     [parent rest-one-profile-panel]
     [label "REST      "])

(let ([choice-widget
       (new choice% [parent rest-one-profile-panel] 
              [label ""] 
              [choices '("very low" "low" "medium" "high" "very high")])])
    ;; Seleccionar el valor inicial como "medium"
    (send choice-widget set-selection 2))

(new message%
     [parent rest-one-profile-panel]
     [label "          "])

(new message%
     [parent rest-one-profile-panel]
     [label "0"]
     [min-width 32])


;;    adult
(define profile-adult-panel
  (new vertical-panel%
       [parent adult-configuration]
       [stretchable-height #f]))

(define eat-two-profile-panel
  (new horizontal-panel%
       [parent profile-adult-panel]))

(new message%
     [parent eat-two-profile-panel]
     [label "EAT        "])

(let ([choice-widget
       (new choice% [parent eat-two-profile-panel] 
              [label ""] 
              [choices '("very low" "low" "medium" "high" "very high")])])
    ;; Seleccionar el valor inicial como "medium"
    (send choice-widget set-selection 2))

(new message%
     [parent eat-two-profile-panel]
     [label "          "])

(new message%
     [parent eat-two-profile-panel]
     [label "0"]
     [min-width 32])


(define work-two-profile-panel
  (new horizontal-panel%
       [parent profile-adult-panel]))

(new message%
     [parent work-two-profile-panel]
     [label "WORK   "])

(let ([choice-widget
       (new choice% [parent work-two-profile-panel] 
              [label ""] 
              [choices '("very low" "low" "medium" "high" "very high")])])
    ;; Seleccionar el valor inicial como "medium"
    (send choice-widget set-selection 2))

(new message%
     [parent work-two-profile-panel]
     [label "          "])

(new message%
     [parent work-two-profile-panel]
     [label "0"]
     [min-width 32])


(define rest-two-profile-panel
  (new horizontal-panel%
       [parent profile-adult-panel]))

(new message%
     [parent rest-two-profile-panel]
     [label "REST      "])

(let ([choice-widget
       (new choice% [parent rest-two-profile-panel] 
              [label ""] 
              [choices '("very low" "low" "medium" "high" "very high")])])
    ;; Seleccionar el valor inicial como "medium"
    (send choice-widget set-selection 2))

(new message%
     [parent rest-two-profile-panel]
     [label "          "])

(new message%
     [parent rest-two-profile-panel]
     [label "0"]
     [min-width 32])


;;    child
(define profile-child-panel
  (new vertical-panel%
       [parent child-configuration]
       [stretchable-height #f]))

(define eat-three-profile-panel
  (new horizontal-panel%
       [parent profile-child-panel]))

(new message%
     [parent eat-three-profile-panel]
     [label "EAT        "])

(let ([choice-widget
       (new choice% [parent eat-three-profile-panel] 
              [label ""] 
              [choices '("very low" "low" "medium" "high" "very high")])])
    ;; Seleccionar el valor inicial como "medium"
    (send choice-widget set-selection 2))

(new message%
     [parent eat-three-profile-panel]
     [label "          "])

(new message%
     [parent eat-three-profile-panel]
     [label "0"]
     [min-width 32])


(define work-three-profile-panel
  (new horizontal-panel%
       [parent profile-child-panel]))

(new message%
     [parent work-three-profile-panel]
     [label "WORK   "])

(let ([choice-widget
       (new choice% [parent work-three-profile-panel] 
              [label ""] 
              [choices '("very low" "low" "medium" "high" "very high")])])
    ;; Seleccionar el valor inicial como "medium"
    (send choice-widget set-selection 2))

(new message%
     [parent work-three-profile-panel]
     [label "          "])

(new message%
     [parent work-three-profile-panel]
     [label "0"]
     [min-width 32])


(define rest-three-profile-panel
  (new horizontal-panel%
       [parent profile-child-panel]))

(new message%
     [parent rest-three-profile-panel]
     [label "REST      "])

(let ([choice-widget
       (new choice% [parent rest-three-profile-panel] 
              [label ""] 
              [choices '("very low" "low" "medium" "high" "very high")])])
    ;; Seleccionar el valor inicial como "medium"
    (send choice-widget set-selection 2))

(new message%
     [parent rest-three-profile-panel]
     [label "          "])

(new message%
     [parent rest-three-profile-panel]
     [label "0"]
     [min-width 32])



;; Interest Levels
(define parent-interest-panel
  (new horizontal-panel%
       [parent parent-configuration])) 

(let ([slider-eating-one
       (new slider%
            [parent parent-interest-panel]
            [min-value 0]
            [max-value 100]
            [label ""]
            [style '(vertical)])]) ; Especifica la orientación vertical
  (void))

(let ([slider-working-one
       (new slider%
            [parent parent-interest-panel]
            [min-value 0]
            [max-value 100]
            [label "  "]
            [style '(vertical)])]) ; Especifica la orientación vertical
  (void))

(let ([slider-resting-one
       (new slider%
            [parent parent-interest-panel]
            [min-value 0]
            [max-value 100]
            [label "  "]
            [style '(vertical)])]) ; Especifica la orientación vertical
  (void))


(define adult-interest-panel
  (new horizontal-panel%
       [parent adult-configuration]))

(let ([slider-eating-two
       (new slider%
            [parent adult-interest-panel]
            [min-value 0]
            [max-value 100]
            [label ""]
            [style '(vertical)])]) ; Especifica la orientación vertical
  (void))

(let ([slider-working-two
       (new slider%
            [parent adult-interest-panel]
            [min-value 0]
            [max-value 100]
            [label "  "]
            [style '(vertical)])]) ; Especifica la orientación vertical
  (void))

(let ([slider-resting-two
       (new slider%
            [parent adult-interest-panel]
            [min-value 0]
            [max-value 100]
            [label "  "]
            [style '(vertical)])]) ; Especifica la orientación vertical
  (void))


(define child-interest-panel
  (new horizontal-panel%
       [parent child-configuration]))

(let ([slider-eating-three
       (new slider%
            [parent child-interest-panel]
            [min-value 0]
            [max-value 100]
            [label ""]
            [style '(vertical)])]) ; Especifica la orientación vertical
  (void))

(let ([slider-working-three
       (new slider%
            [parent child-interest-panel]
            [min-value 0]
            [max-value 100]
            [label "  "]
            [style '(vertical)])]) ; Especifica la orientación vertical
  (void))

(let ([slider-resting-three
       (new slider%
            [parent child-interest-panel]
            [min-value 0]
            [max-value 100]
            [label "  "]
            [style '(vertical)])]) ; Especifica la orientación vertical
  (void))




; Visualización
(define visual-panel
  (new vertical-panel%
       [parent bottom-panel]
       [stretchable-height #t]
       [alignment '(left top)]))


;; Agentes
(define agent-panel
  (new horizontal-panel%
       [parent visual-panel]
       [stretchable-height #t]
       [alignment '(left top)]))


(define agente1 (new agent% [id 1] [is-red #t] [energia-inicial 300] [posicion-inicial '(50 50)] [area-vision 100]))
(define agente2 (new agent% [id 2] [is-red #f] [energia-inicial 150] [posicion-inicial '(250 150)] [area-vision 100]))
(define agentes (list agente1 agente2))

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

(define main-agent-panel
  (new horizontal-panel%
       [parent agent-panel]
       [spacing 0]
       [stretchable-width #f]
       [stretchable-height #f]
       [alignment '(left top)]))


(draw-factory-items main-agent-panel)


(define agent-right-panel
    (new vertical-panel%
         [parent main-agent-panel]
         [spacing 0]))

(define factory-canvas
    (new canvas%
         [parent agent-right-panel]
         [min-width 360]
         [min-height 280]
         [paint-callback
          (λ (canvas dc)
            (draw-factory-canvas canvas dc agentes rechargeZones machines))]))

;; Temporizador para mover los agentes
(define temporizador
  (new timer%
       [notify-callback
        (λ ()
          (if (ormap (λ (agente) (> (send agente estado-energia) 0)) agentes)
              (begin
                (for-each (λ (agente) (send agente bucle-principal (send factory-canvas get-dc) objects)) agentes)
                (send factory-canvas refresh)
                (for-each (λ (agente) (send agente reducir-energia)) agentes))
              (begin
                (send temporizador stop)
                (printf "Todos los agentes han agotado su energía. Deteniendo ciclo.\n"))))]))




;; Histogramas
(define histogram-config-panel
  (new horizontal-panel%
       [parent visual-panel]
       [stretchable-height #t]
       [alignment '(left top)]))


(define histogram-panel
  (new horizontal-panel%
       [parent histogram-config-panel]
       [stretchable-height #t]
       [alignment '(left top)]))

(draw-histograms histogram-panel)


(define histogram-indicator-panel
  (new vertical-panel%
       [parent histogram-config-panel]
       [stretchable-height #t]
       [alignment '(left center)]))

(new message% [parent histogram-indicator-panel] [label "    ACTION"])

(define action-field
  (new text-field%
       [parent histogram-indicator-panel]
       [label ""]
       [init-value "WORKING"]
       [min-width 50]
       [enabled #f]))



;; Mostrar la ventana
(send frame show #t)
