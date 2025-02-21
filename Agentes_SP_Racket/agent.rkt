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
    ;(define energiaMaxima energia-inicial)
    (define energiaMaxima 250)
    (define posicion (box posicion-inicial))
    (define areaVision (box area-vision))
    (define orientacion (box (random 360)))
    (define lista-objetos (box '()))
    (define estado (box (new sin-evento%)))
    (define objetivo (box '()))

    
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

;; Función para verificar colisión con una pared
    (define/public (mover-adelante dc objects)
      (let* ([orientacionAnterior (unbox orientacion)]
             [rad (/ (* (unbox orientacion) pi) 180)]
             [pos (unbox posicion)]
             [nuevaPosX (+ (first pos) (* 5 (cos rad)))]
             [nuevaPosY (+ (second pos) (* 5 (sin rad)))]
             [nuevaPosicion (list nuevaPosX nuevaPosY)]

             ;; Definir los límites del área de dibujo
             [limiteIzquierdo 10]
             [limiteDerecho 350]
             [limiteSuperior 10]
             [limiteInferior 270]

             ;; Verificar si la nueva posición está fuera de los límites
             [fueraDeLimites
              (or (< nuevaPosX limiteIzquierdo)
                  (> nuevaPosX limiteDerecho)
                  (< nuevaPosY limiteSuperior)
                  (> nuevaPosY limiteInferior))]

             ;; Verificar colisiones con objetos
             [colision-objeto (findf (λ (obj) (colision? nuevaPosicion obj)) objects)])

        (cond
          ;; Si la nueva posición está fuera de los límites
          [fueraDeLimites
           (begin
             ;; Ajustar la posición para mantener al agente dentro de los límites
             (set-box! posicion
                       (list (min (max nuevaPosX limiteIzquierdo) limiteDerecho)
                             (min (max nuevaPosY limiteSuperior) limiteInferior)))
             ;; Cambiar la orientación para evitar que el agente siga moviéndose hacia fuera
             (set-box! orientacion (+ (+ (random 180) 90) orientacionAnterior))
             (printf "Agente ~a: Fuera de límites, cambiando orientación.\n" id)
             ;; Intentar moverse nuevamente
             (send this mover-adelante dc objects))]

          ;; Si colisiona con un objeto
          [colision-objeto
           (begin
             ;; Cambiar la orientación para evitar la colisión
             (set-box! orientacion (+ (+ (random 180) 90) orientacionAnterior))
             (printf "Agente ~a: Colisión con objeto, cambiando orientación.\n" id)
             ;; Intentar moverse nuevamente
             (send this mover-adelante dc objects))]

          ;; Movimiento válido
          [else
           (begin
             ;; Actualizar la posición del agente
             (set-box! posicion nuevaPosicion)
             (printf "Nueva posición: ~a\n" nuevaPosicion)
             (printf "Agente ~a: Se movió hacia adelante.\n" id))])))


    
    (define/public (mover-hacia-objeto dc object objects)
      (let* ([pos (unbox posicion)]
             [objPos (send object obtener-posicion)]
             [destino (calcular-punto-cercano pos objPos)]
             [dx (- (first destino) (first pos))]
             [dy (- (second destino) (second pos))]
             [dist (sqrt (+ (sqr dx) (sqr dy)))]
             [nuevaPosX (+ (first pos) (if (> dist 0) (* (/ dx dist) 5) 0))]
             [nuevaPosY (+ (second pos) (if (> dist 0) (* (/ dy dist) 5) 0))]
             [nuevaPosicion (list nuevaPosX nuevaPosY)]
             [limiteIzquierdo 10]
             [limiteDerecho 350]
             [limiteSuperior 10]
             [limiteInferior 270]
             [dentroLimites
              (and (>= nuevaPosX limiteIzquierdo)
                   (<= nuevaPosX limiteDerecho)
                   (>= nuevaPosY limiteSuperior)
                   (<= nuevaPosY limiteInferior))]
             [colision (findf (λ (obj) (colision? nuevaPosicion obj)) objects)]
             ;; Detección de esquinas
             [esquina-superior-izquierda (and (= nuevaPosX limiteIzquierdo) (= nuevaPosY limiteSuperior))]
             [esquina-superior-derecha (and (= nuevaPosX (- limiteDerecho 10)) (= nuevaPosY limiteSuperior))]
             [esquina-inferior-izquierda (and (= nuevaPosX limiteIzquierdo) (= nuevaPosY (- limiteInferior 10)))]
             [esquina-inferior-derecha (and (= nuevaPosX (- limiteDerecho 10)) (= nuevaPosY (- limiteInferior 10)))]

             [registrados (unbox lista-objetos)])

        (cond
          ;; Si está en una esquina, cambiar la orientación
          [(or esquina-superior-izquierda esquina-superior-derecha esquina-inferior-izquierda esquina-inferior-derecha)
           (begin
             (printf "Atascado en una esquina. Cambiando orientación.\n")
             (set-box! orientacion (random 360))
             (send this mover-hacia-objeto dc object objects))]

          ;; Fuera de los límites
          [(not dentroLimites)
           (begin
             (printf "Fuera de límites. Cambiando orientación.\n")
             (set-box! orientacion (random 120 270))
             (send this mover-hacia-objeto dc object objects))]

          ;; Colisiona con un objeto no deseado
          [(and colision (not (equal? colision object))) ; No es el objetivo
           (cond
             [(is-in-recharge-zone nuevaPosicion colision)
              (printf "Pasando a traves de la zona de recarga ~a\n" (send colision obtener-id))
              (set-box! posicion nuevaPosicion)
              (printf "Nueva posición: ~a\n" nuevaPosicion)]
             [else
              (printf "COLISION con objeto no deseado: ~a\n" (send colision obtener-id))
              (rodear-objeto pos colision)
              (send this mover-hacia-objeto dc object objects)])]

          ;; Movimiento válido
          [else
           (set-box! posicion nuevaPosicion)
           (printf "Nueva posición: ~a\n" nuevaPosicion)
           (cond
             ;; Llega al objetivo tipo "máquina"
             [(and (colision? nuevaPosicion object) (isMachine? object))
              (begin
                (printf "Agente ~a: Llegó a la máquina ~a.\n" id (send object obtener-id))
                (send this registrarObjeto object)
                (set-box! objetivo (remove object (unbox objetivo)))
                (eliminar-objeto object objects) ; Eliminar objeto de la lista
                (mover-fuera-del-objeto pos object))]

             ;; Llega a una zona de recarga
             [(is-in-recharge-zone nuevaPosicion object)
              (begin
                (printf "Agente ~a: Llegó a la zona de recarga ~a.\n" id (send object obtener-id))
                (send this registrarObjeto object)
                (set-box! objetivo (remove object (unbox objetivo)))
                (eliminar-objeto object objects))]

             ;; Moviéndose hacia el destino
             [else
              (if (member object registrados)
                  (printf "Agente ~a: Moviéndose hacia ~a ~a.\n" id (send object obtener-tipo) (send object obtener-id))
                  (printf "Agente ~a: Moviéndose hacia ~a.\n" id (send object obtener-tipo))
                  )
              ])])))


    ;; Función para eliminar el objeto de la lista
    (define (eliminar-objeto object objects)
      (set! objects (remove (λ (obj) (equal? obj object)) objects)))

    ;; Función para mover fuera del objeto después de registrarlo
    (define (mover-fuera-del-objeto pos object)
      (let* ([objPos (send object obtener-posicion)]
             [ox (first objPos)]
             [oy (second objPos)]
             [ow (third objPos)]
             [oh (fourth objPos)]
             [px (first pos)]
             [py (second pos)]
             [desplazamiento 10] ; Cantidad para ajustar la posición
             [nuevaPos
              (cond
                ;; Si está a la izquierda del objeto
                [(< px ox) (list (- px desplazamiento) py)]
                ;; Si está a la derecha del objeto
                [(> px (+ ox ow)) (list (+ px desplazamiento) py)]
                ;; Si está arriba del objeto
                [(< py oy) (list px (- py desplazamiento))]
                ;; Si está debajo del objeto
                [(> py (+ oy oh)) (list px (+ py desplazamiento))]
                ;; Caso por defecto (sin ajuste)
                [else pos])])
        (set-box! posicion nuevaPos)))

    ;; Función de colisión
    (define (colision? pos obj)
      (let* ([x (first pos)]
             [y (second pos)]
             [objPos (send obj obtener-posicion)]
             [ox (first objPos)]
             [oy (second objPos)]
             [ow (third objPos)]
             [oh (fourth objPos)])
        (and (<= ox x (+ ox ow))
             (<= oy y (+ oy oh)))))



        ;; Ajustar posición al rodear un objeto
    (define (rodear-objeto pos obj)
      (let* ([objPos (send obj obtener-posicion)]
             [ox (first objPos)]
             [oy (second objPos)]
             [ow (third objPos)]
             [oh (fourth objPos)]
             [px (first pos)]
             [py (second pos)]
             [desplazamiento 10] ; Cantidad para ajustar la posición
             [nuevaPos
              (cond
                ;; Si está a la izquierda del objeto
                [(< px ox) (list (- px desplazamiento) py)]
                ;; Si está a la derecha del objeto
                [(> px (+ ox ow)) (list (+ px desplazamiento) py)]
                ;; Si está arriba del objeto
                [(< py oy) (list px (- py desplazamiento))]
                ;; Si está debajo del objeto
                [(> py (+ oy oh)) (list px (+ py desplazamiento))]
                ;; Caso por defecto (sin ajuste)
                [else pos])])
        (set-box! posicion nuevaPos)))


    ;; Función para calcular el punto más cercano a un objeto
    (define (calcular-punto-cercano pos objPos)
      (let* ([px (first pos)]
             [py (second pos)]
             [ox (first objPos)]
             [oy (second objPos)]
             [ow (third objPos)]
             [oh (fourth objPos)]
             [nx (min (max px ox) (+ ox ow))]
             [ny (min (max py oy) (+ oy oh))])
        (list nx ny)))
    

    (define/public (buscar dc objects)
      (let* ([pos (unbox posicion)]
             [vision (unbox areaVision)]
             [objetosCercanos
              (filter (λ (object) (enRangoVision pos vision object)) objects)]
             [registrados (unbox lista-objetos)]
             [objetoDetectado (box 0)])
    
        (begin
          (printf "OBJETOS CERCANOS:")
          (for ([object objetosCercanos])
            (printf " ~a" (send object obtener-id)))
          (printf "\n")
          (if (empty? objetosCercanos)
            (begin
              (printf "Agente ~a: No encontró objetos cercanos.\n" id)
              (send this mover-adelante dc objects)
            )
            (for ([object objetosCercanos])
              (if (member object registrados)
                  (begin
                    (printf "Agente ~a: Ya conoce el objeto ~a.\n" id (send object obtener-tipo))
                    (send this mover-adelante dc objects)
                  )
                  (cond
                    [(= (unbox objetoDetectado) 0)
                     (printf "Agente ~a: Nuevo objeto encontrado. Moviéndose hacia ~a.\n" id (send object obtener-tipo))
                     (set-box! objetivo (cons object (unbox objetivo))) ; Agrega el objeto a la lista de objetivos
                     (send this mover-hacia-objeto dc object objects)
                     (set-box! objetoDetectado (+ (unbox objetoDetectado) 1))]
                  ))))
          )
        ))


    
    (define/public (comer)
      (let* ([energia-actual (unbox energia)]
             [nueva-energia (min (+ energia-actual 50) energiaMaxima)])
        (set-box! energia nueva-energia)
        (printf "Agente ~a: Recargó energía. Energía actual: ~a\n" id nueva-energia)
        (if (>= nueva-energia energiaMaxima)
            (begin
              (printf "Agente ~a: Energía completamente recargada\n" id)
              (set-box! estado sin-evento%)
            )
            (begin
              (printf "Agente ~a: Energía recargada parcialmente\n" id)
            )
          )
        )
      )

    
    
    ;; ###GESTOR DE EVENTOS###

    (define/public (bucle-principal dc objects)
      (when (> (unbox energia) 0)
        (send this gestionar-eventos dc objects)
        ))


    (define/public (gestionar-eventos dc objects)
      (let ([energiaActual (unbox energia)]
            [estadoAgente (unbox estado)]
            [object (if (empty? (unbox objetivo))
                  '() ; Si la lista está vacía, evita el error
                  (first (unbox objetivo)))])
        (cond
          [(<= energiaActual 0)
           (send this bucle-principal dc objects)
          ]

          [(is-a? estadoAgente evento-activo%)
           (printf "Agente ~a: Continuando en estado Hambre.\n" id)
           (if (empty? object)
               (send this realizar-evento-hambre dc objects)
               (send this mover-hacia-objeto dc object objects))
          ]

          [(< energiaActual umbral-hambre)
           (printf "Agente ~a: Cambiando estado a Hambre.\n" id)
           (set-box! estado (crear-evento-hambre))
           (if (empty? object)
               (send this realizar-evento-hambre dc objects)
               (send this mover-hacia-objeto dc object objects))
          ]
          
          [else
           (printf "Agente ~a: Explorando.\n" id)
           (if (empty? object)
               (send this explorar dc objects)
               (send this mover-hacia-objeto dc object objects))
          ]
        )
      )
    )

    
    
    ;; ###EVENTOS###
    
    (define/public (realizar-evento-hambre dc objects)
      (let ([registrados (unbox lista-objetos)])
        (cond
          [(empty? registrados)
           (printf "Agente ~a: No tiene objetos registrados. Buscando objetos cercanos.\n" id)
           (send this buscar dc objects)]
          [else
           (if (hay-objeto? "Zona de Recarga" registrados)
               (begin
                 (let* ([objeto-destino (first (filter isRechargeZone? registrados))])
                   (cond
                     [(is-in-recharge-zone (unbox posicion) objeto-destino)
                      (send this comer)]
                     [else
                      (begin
                        (set-box! objetivo (cons objeto-destino (unbox objetivo)))
                        (send this mover-hacia-objeto dc objeto-destino objects)
                        ;; Verifica colisiones
                        (let* ([nuevaPosicion (unbox posicion)]
                               [colision (findf (λ (obj) (colision? nuevaPosicion obj)) objects)])
                          (cond
                            ;; Caso: Llega a una zona de recarga
                            [(and (not (empty? colision))
                                  (is-in-recharge-zone nuevaPosicion objeto-destino))
                             (send this comer)]
                            )))])
               ))
               (begin
                 (printf "Agente ~a: No tiene zonas de recarga registradas. Comenzando a buscar\n" id)
                 (send this buscar dc objects)
               )
             )
           ]
          )
        )
      )

    
    (define/public (explorar dc objects)
      (let* ([pos (unbox posicion)]
             [vision (unbox areaVision)]
             [objetosEnRango (filter (λ (object) (enRangoVision pos vision object)) objects)]
             [registrados (unbox lista-objetos)]
             [objetoDetectado (box 0)])

        (cond
          ;; Si no hay objetos en rango
          [(empty? objetosEnRango)
           (printf "Agente ~a: Sigue explorando.\n" id)
           (send this mover-adelante dc objects)]

          [(= (length registrados) (length objects))
           (printf "Agente ~a: Ya tiene todos los objetos registrados.\n" id)
           (send this mover-adelante dc objects)
           ]

           ;; Si todos los objetos en rango ya están registrados
          [(and (not (empty? objetosEnRango))
                (every? (λ (object) (objetoRegistrado? object registrados)) objetosEnRango))
           (printf "Agente ~a: Todos los objetos en rango ya están registrados, avanzando.\n" id)
           (send this mover-adelante dc objects)]
      
          ;; Si hay objetos en rango
          [else
           (for-each
            (λ (object)
              (if (objetoRegistrado? object registrados)
                  (printf "Agente ~a: Objeto ya conocido, continuando revisando lista de registrados.\n" id)
                  (cond
                    [(= (unbox objetoDetectado) 0)
                     (printf "Agente ~a: Objeto detectado, caminando hacia objeto.\n" id)
                     (set-box! objetivo (cons object (unbox objetivo)))
                     (send this mover-hacia-objeto dc object objects)
                     (set-box! objetoDetectado (+ (unbox objetoDetectado) 1))]
                    )
                  )
             ) objetosEnRango)]
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
    

    (define (enRangoVision pos areaVision obj)
      (let* ([x (first pos)]
             [y (second pos)]
             [radio (/ areaVision 2)]  ; Radio del área de visión
             [objpos (send obj obtener-posicion)]
             [ox (first objpos)]
             [oy (second objpos)]
             [ow (third objpos)]
             [oh (fourth objpos)]
             [esquina-izquierda-superior (list ox oy)]
             [esquina-derecha-superior (list (+ ox ow) oy)]
             [esquina-izquierda-inferior (list ox (+ oy oh))]
             [esquina-derecha-inferior (list (+ ox ow) (+ oy oh))]

             ;; Definir función de distancia dentro del let*
             [distancia (lambda (p1 p2)
                          (sqrt (+ (sqr (- (first p1) (first p2)))
                                   (sqr (- (second p1) (second p2))))))]

             ;; Verificar si alguna esquina está dentro del área de visión
             [dentro-vision
              (or (<= (distancia (list x y) esquina-izquierda-superior) radio)
                  (<= (distancia (list x y) esquina-derecha-superior) radio)
                  (<= (distancia (list x y) esquina-izquierda-inferior) radio)
                  (<= (distancia (list x y) esquina-derecha-inferior) radio))])
        dentro-vision))




    
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

    (define (every? pred lst)
      (cond
        [(empty? lst) #t] ; Si la lista está vacía, todos los elementos cumplen el predicado
        [(not (pred (first lst))) #f] ; Si el primer elemento no cumple, retorna #f
        [else (every? pred (rest lst))])) ; Sigue evaluando el resto de la lista
    )
  )