#lang racket/gui

;; #### INFORMACIÓN BASE ####

;; Definir el estado del agente como estructuras independientes
(struct sin-evento () #:transparent)
(struct evento-activo (tipo) #:transparent)

;; Definir el evento "hambre"
(define (evento-hambre) 'hambre)


;; Función para verificar si un estado pertenece a los estados del agente
(define (estado-agente? estado)
  (or (sin-evento? estado) 
      (evento-activo? estado)))

;; Umbral de energía que desencadena el evento de hambre
(define umbral-hambre 170)
(define vida-maxima 200)

;; Estructura del agente
(struct Agente (id is-red energia posicion orientacion roles lista-objetos area-vision estado) #:transparent)

;; Crear un agente
(define (crear-agente id is-red energia-inicial posicion-inicial area-vision)
  (Agente id
          is-red
          (box energia-inicial) ;; La energía es mutable
          (box posicion-inicial) ;; La posición es mutable
          (box (random 360)) ;; Orientación inicial aleatoria
          '() ;; Lista de roles
          (box '()) ;; Lista de objetos encontrados
          area-vision
          (box (sin-evento)))) ;; Estado inicial

;; #### FUNCIONES AUXILIARES ####

;; Reducir energía del agente
(define (reducir-energia agente)
  (let ([energia (unbox (Agente-energia agente))])
    (if (> energia 0)
        (set-box! (Agente-energia agente) (sub1 energia))
        (set-box! (Agente-estado agente) (sin-evento)))))

;; Mover al agente hacia un objetivo
(define (mover-hacia-objetivo agente objetivo)
  (let* ([pos (unbox (Agente-posicion agente))]
         [dx (- (first objetivo) (first pos))]
         [dy (- (second objetivo) (second pos))]
         [dist (sqrt (+ (sqr dx) (sqr dy)))]
         [paso-x (if (> dist 0) (* (/ dx dist) 5) 0)]
         [paso-y (if (> dist 0) (* (/ dy dist) 5) 0)])
    (set-box! (Agente-posicion agente)
              (list (+ (first pos) paso-x)
                    (+ (second pos) paso-y)))))

;; Explorar el entorno
(define (explorar agente)
  (let* ([orientacion (unbox (Agente-orientacion agente))]
         [rad (/ (* orientacion pi) 180)]
         [pos (unbox (Agente-posicion agente))]
         [paso-x (* 5 (cos rad))]
         [paso-y (* 5 (sin rad))])
    (set-box! (Agente-posicion agente)
              (list (+ (first pos) paso-x)
                    (+ (second pos) paso-y)))))

;; Gestionar eventos del agente
(define (gestionar-eventos agente)
  (let ([energia (unbox (Agente-energia agente))]
        [estado (unbox (Agente-estado agente))])
    (cond
      [(<= energia 0) (printf "El agente ~a ha muerto.\n" (Agente-id agente))]
      [(and (< energia umbral-hambre) (estado-agente? estado))
       (set-box! (Agente-estado agente) (evento-activo (evento-hambre)))
       (printf "El agente ~a tiene hambre. Cambiando estado.\n" (Agente-id agente))]
      [else
       (explorar agente)
       (printf "El agente ~a está explorando.\n" (Agente-id agente))])))

;; #### BUCLE PRINCIPAL ####

(define (bucle-principal agente)
  (when (> (unbox (Agente-energia agente)) 0)
    (gestionar-eventos agente)
    (reducir-energia agente)
    (bucle-principal agente)))

;; #### EJECUCIÓN ####

;; Crear un agente y ejecutar el bucle principal
(define agente-prueba (crear-agente 1 #t 200 '(100 100) 50))
(bucle-principal agente-prueba)
