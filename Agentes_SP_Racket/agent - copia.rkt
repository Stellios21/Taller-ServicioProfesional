#lang racket/gui

(require "rechargeZone.rkt")
(require "machine.rkt")

;; #### INFORMACIÓN BASE ####

;; Clase para el estado "sin-evento"
(define sin-evento%
  (class object%
    (super-new)))

;; Clase para el estado "evento-activo"
(define evento-activo%
  (class object%
    (init-field tipo)
    (super-new)))

;; Función para crear un evento de hambre
(define (crear-evento-hambre)
  (new evento-activo% [tipo 'hambre]))

;; Función para verificar si un objeto es un estado válido del agente
(define (estado-agent? estado)
  (or (is-a? estado sin-evento%)
      (is-a? estado evento-activo%)))

;; Umbral de energía que desencadena el evento de hambre
(define umbral-hambre 170)



(provide agent%)

;; Clase Agente
(define agent%
  (class object%

    ;; Constructor
    (init-field id is-red energia-inicial posicion-inicial area-vision)
    (super-new)

    ; Variables internas
    (define energia (box energia-inicial))
    (define energiaMaxima energia-inicial)
    (define posicion (box posicion-inicial))
    (define areaVision (box area-vision))
    (define orientacion (box (random 360)))
    (define lista-objetos (box '()))
    (define estado (box (new sin-evento%)))

    
    ;; Dibujar agente
    (define/public (dibujar dc)
      (let* ([x (first (unbox posicion))]
             [y (second (unbox posicion))]
             [vision (unbox areaVision)]
             [vision-offset (/ vision 2)]) ; Calcular el desplazamiento para centrar el círculo
        ;; Dibujar el área de visión
        (send dc set-pen (make-object pen% "black" 2 'solid))
        (send dc set-brush (make-object brush% "white" 'transparent))
        (send dc draw-ellipse (- x vision-offset) 
              (- y vision-offset) 
              vision 
              vision)

        ;; Dibujar el agente
        (send dc set-brush (make-object brush% (if is-red "red" "blue") 'solid))
        (send dc draw-ellipse (- x 10) ; Ajustar para que el agente quede centrado
              (- y 10)
              20 
              20)))


          
            
    ;; ### ACCIONES ###
    
    (define/public (mover-hacia-objeto dc object objects)
      (let* ([pos (unbox posicion)]
             [posObj (send object obtener-posicion)] ; Supone que `object` tiene un método `obtener-posicion`.
             [dx (- (first posObj) (first pos))]
             [dy (- (second posObj) (second pos))]
             [dist (sqrt (+ (sqr dx) (sqr dy)))]
             [paso-x (if (> dist 0) (* (/ dx dist) 5) 0)]
             [paso-y (if (> dist 0) (* (/ dy dist) 5) 0)]
             [nuevaPosicion (list (+ (first pos) paso-x)
                                  (+ (second pos) paso-y))])

        ;; Actualiza la posición del agente
        (set-box! posicion nuevaPosicion)

        ;; Verifica colisiones
        (let ([colision (findf (λ (obj) (colision? nuevaPosicion obj)) objects)])
          (cond
            ;; Caso: Llega a una máquina
            [(and (not (empty? colision))
                  (isMachine? object))
             (printf "Agente ~a: Llegó a la máquina ~a.\n" id (send object obtener-id))]

            ;; Caso: Llega a una zona de recarga
            [(and (not (empty? colision))
                  (is-in-recharge-zone nuevaPosicion object))
             (printf "Agente ~a: Llegó a la zona de recarga ~a.\n" id (send object obtener-id))]

            ;; Caso: Sigue moviéndose hacia el objeto
            [else
             (printf "Agente ~a: Moviéndose hacia una ~a.\n" id (send object obtener-tipo))
             (send this dibujar dc)

             (send this reducir-energia)
             
             (let ([energiaActual (unbox energia)])
               (if (<= energiaActual 0)
                   (send this bucle-principal dc objects)
                   (send this mover-hacia-objeto dc object objects)))
             ]))))


    (define/public (buscar dc objects)
      (let* ([pos (unbox posicion)]
             [vision (unbox areaVision)]
             [objetosCercanos
              (filter (λ (object) (enRangoVision pos vision object)) objects)]
             [registrados (unbox lista-objetos)])
    
        (if (empty? objetosCercanos)
            (printf "Agente ~a: No encontró objetos cercanos.\n" id)
            (for ([object objetosCercanos])
              (if (member object registrados)
                  (printf "Agente ~a: Ya conoce el objeto ~a.\n" id (send object obtener-tipo))
                  (begin
                    (printf "Agente ~a: Nuevo objeto encontrado. Moviéndose hacia ~a.\n" id (send object obtener-tipo))
                    (send this mover-hacia-objeto dc object objects)
                    (send this registrarObjeto object)))))))


    
    (define/public (comer dc)
      (let* ([energia-actual (unbox energia)]
             [nueva-energia (min (+ energia-actual 50) (unbox energiaMaxima))])
        (set-box! energia nueva-energia)
        (printf "Agente ~a: Recargó energía. Energía actual: ~a\n" id nueva-energia)
        (if (equal? nueva-energia (unbox energiaMaxima))
            (begin
              (printf "Agente ~a : Energía completamente recargada\n" id)
              (set-box! estado sin-evento%)
            )
            (begin
              (printf "Agente ~a : Energía recargada parcialmente\n" id)
              (send this comer dc)
            )
          )
        )
      )

    
    
    ;; ###GESTOR DE EVENTOS###

    (define/public (bucle-principal dc objects)
      (when (> (unbox energia) 0)
        (send this gestionar-eventos dc objects)
        (send this bucle-principal dc objects)
        ))


    (define/public (gestionar-eventos dc objects)
      (send this reducir-energia)

      (let ([energiaActual (unbox energia)]
            [estadoAgente (unbox estado)])
        (cond
          [(<= energiaActual 0)
           (send this bucle-principal dc objects)]
          [(is-a? estadoAgente sin-evento%)
           ; Verificar si el agente tiene hambre
           (if (< energiaActual umbral-hambre)
               (begin
                 (printf "El agente ~a tiene hambre. Cambiando estado a Hambre.\n" id)
                 (set-box! estado (crear-evento-hambre))
                 (send this realizar-evento-hambre dc objects)
                 )
               (send this explorar dc objects)
           )]
          [else
           (printf "El agente ~a está explorando.\n" id)
           (send this explorar dc objects)
           ]
          )
        )
      )

    
    
    ;; ###EVENTOS###
    
    (define/public (realizar-evento-hambre dc objects)
      (let ([registrados (unbox lista-objetos)])
        (cond
          [(empty? registrados)
           (printf "Agente ~a no tiene objetos registrados. Buscando objetos cercanos.\n" id)
           (send this buscar dc objects)]
          [else
           (if (hay-objeto? "Zona de Recarga" registrados)
               (begin
                 (let ([objeto-destino (first (filter isRechargeZone? objects))])
                   (send this mover-hacia-objeto dc objeto-destino objects))
                 
                 (comer dc objects)
                 (bucle-principal dc objects)
                )
               (begin
                 (printf "Agente ~a llegó a una máquina, pero no puede recargar aqui.\n" id)
                 (send this buscar dc objects)
               )
             )
           ]
          )
        )
      )

    
    (define/public (explorar dc objects)
      (let* ([rad (/ (* (unbox orientacion) pi) 180)]
             [pos (unbox posicion)]
             [nuevaPosX (+ (first pos) (* 5 (cos rad)))]
             [nuevaPosY (+ (second pos) (* 5 (sin rad)))]
             [nuevaPosicion (list nuevaPosX nuevaPosY)]

             [limiteIzquierdo 10]
             [limiteDerecho 350]
             [limiteSuperior 10]
             [limiteInferior 270]

             ;; Verificar si está dentro de los límites
             [dentroLimites
              (and (>= nuevaPosX limiteIzquierdo)
                   (<= nuevaPosX limiteDerecho)
                   (>= nuevaPosY limiteSuperior)
                   (<= nuevaPosY limiteInferior))]

             ;; Verificar colisiones
             [colision (findf (λ (obj) (colision? nuevaPosicion obj)) objects)])

        (cond
          ;; Si está fuera de los límites
          [(not dentroLimites)
           (begin
             (set-box! orientacion (random 120 270))
             (send this bucle-principal dc objects))]
          
          ;; Si colisiona con un objeto
          [(if colision
               (begin
                  (set-box! orientacion (random 120 270))
                  (send this explorar dc objects)
               )
               (begin
                 (set-box! posicion nuevaPosicion)

                 (send this reducir-energia)

                 (let ([energiaActual (unbox energia)])
                   (cond [(<= energiaActual 0)
                          (printf "Hola \n")
                          (send this bucle-principal dc objects)
                          ]))
                 
                 (send this dibujar dc)
                 
                 (let* ([vision (unbox areaVision)]
                        [objetosEnRango (filter (λ (object) (enRangoVision nuevaPosicion vision object)) objects)]
                        [registrados (unbox lista-objetos)])

                   (cond
                     ;; Si no hay objetos en rango
                     [(empty? objetosEnRango)
                      (send this bucle-principal dc objects)]

                     ;; Si hay objetos en rango
                     [else
                      (for-each
                       (λ (object)
                         (if (objetoRegistrado? object registrados)
                             (printf "Agente ~a: Objeto ya conocido, continuando movimiento en línea recta.\n" id)
                             (begin
                               (printf "Agente ~a: Objeto detectado, caminando hacia objeto.\n" id)
                               (send this mover-hacia-objeto dc object objects)

                               (send this registrarObjeto object))))
                       objetosEnRango)
                      (send this bucle-principal dc objects)]
                     )
                   )
                 )
               )]
          )
        )
      )

    

    

    ;; ###FUNCIONES AUXILIARES###

    (define/public (reducir-energia)
      (let* ([energiaActual (unbox energia)]
             [nuevaEnergia (sub1 energiaActual)]) ; Reducir energía en 1
        (set-box! energia nuevaEnergia) ; Actualizar la energía en la caja
        (if (<= nuevaEnergia 0) ; Si la energía es 0 o menos
            (begin
              (printf "Agente ~a ha muerto por falta de energia.\n" id)
              (set-box! estado (new sin-evento%)) ; Cambiar estado del agente
            ) 
            (printf "Agente ~a tiene ~a de energía.\n" id nuevaEnergia)
        )
      )
    )


    (define (hay-objeto? tipo lista-objetos)
      (ormap (λ (objeto) (equal? (send objeto obtener-tipo) tipo)) lista-objetos))

    
    (define (colision? pos obj)
      (let* ([x (first pos)]
             [y (second pos)]
             [objpos (send obj obtener-posicion)]
             [ox (first objpos)]
             [oy (second objpos)]
             [ow (third objpos)]
             [oh (fourth objpos)]
             [os (send obj obtener-tipo)]
             [agentWidth 10] ; Ajusta esto según el tamaño del agente
             [agentHeight 10]) ; Ajusta esto según el tamaño del agente
        (and (<= ox (+ x agentWidth)) ; Borde izquierdo del objeto toca o está dentro del agente
             (>= (+ ox ow) x)         ; Borde derecho del objeto toca o está dentro del agente
             (<= oy (+ y agentHeight)) ; Borde superior del objeto toca o está dentro del agente
             (>= (+ oy oh) y)          ; Borde inferior del objeto toca o está dentro del agente
             (string=? "Maquina" os)))) ; Compara exactamente el tipo


    

    (define (enRangoVision pos areaVision obj)
      (let* ([x (first pos)]
             [y (second pos)]
             [objpos (send obj obtener-posicion)]
             [ox (first objpos)]
             [oy (second objpos)]
             [ow (third objpos)]
             [oh (fourth objpos)]
             ;; Calcular las distancias a los cuatro lados del rectángulo
             [distanciaIzquierda (sqrt (+ (sqr (- ox x)) (sqr (- oy y))))]
             [distanciaDerecha (sqrt (+ (sqr (- (+ ox ow) x)) (sqr (- oy y))))]
             [distanciaSuperior (sqrt (+ (sqr (- ox x)) (sqr (- (+ oy oh) y))))]
             [distanciaInferior (sqrt (+ (sqr (- (+ ox ow) x)) (sqr (- (+ oy oh) y))))])
        ;; Verificar si alguna distancia está dentro del rango de visión
        (or (<= distanciaIzquierda areaVision)
            (<= distanciaDerecha areaVision)
            (<= distanciaSuperior areaVision)
            (<= distanciaInferior areaVision))))


    
    (define (objetoRegistrado? object registrados)
      (member object registrados))


    (define/public (estado-energia)
      (unbox energia))
    
    (define/public (estado-posicion)
      (unbox posicion))


    (define/public (registrarObjeto objeto)
      (set-box! lista-objetos (cons objeto (unbox lista-objetos)))
      (printf "Agente ~a ha registrado el objeto ~a.\n" id (send objeto obtener-id))
    )

    (define (isMachine? object)
      (equal? (send object obtener-tipo) "Maquina"))

    (define (isRechargeZone? object)
      (equal? (send object obtener-tipo) "Zona de Recarga"))

    
    (define (is-in-recharge-zone pos obj)
      (let* ([x (first pos)]
             [y (second pos)]
             [objpos (send obj obtener-posicion)]
             [ox (first objpos)]
             [oy (second objpos)]
             [ow (third objpos)]
             [oh (fourth objpos)]
             [os (send obj obtener-tipo)]
             [agentWidth 10] ; Ajusta esto según el tamaño del agente
             [agentHeight 10]) ; Ajusta esto según el tamaño del agente
        (and (<= ox (+ x agentWidth)) ; Borde izquierdo del objeto toca o está dentro del agente
             (>= (+ ox ow) x)         ; Borde derecho del objeto toca o está dentro del agente
             (<= oy (+ y agentHeight)) ; Borde superior del objeto toca o está dentro del agente
             (>= (+ oy oh) y)          ; Borde inferior del objeto toca o está dentro del agente
             (string=? "Zona de Recarga" os)))) ; Compara exactamente el tipo
    )
  )
 