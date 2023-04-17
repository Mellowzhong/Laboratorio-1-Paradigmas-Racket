#lang racket
(require "TDA-System.rkt")
(require "Fecha.rkt")
;-----------------------Representacion-----------------------
;Se presenta el TDA User, el cual corresponde tal y como indica su nombre a una representacion
;del usuario, la cual contiene el nombre y el estado de "logeo". Esta representacion esta dada
;por una lista de esos elementos los cuales se modificaran dependiendo de lo que se le pida.

;-----------------------Modificadores-----------------------
;Dominio: sys (list) - user (string)
;Recorrido: system (list)
;Descripción: Añade un usuario al sistema
;Tipo de recursion: No empleada
(define add-user (lambda (sys  user)
    (if (filterElement (getUser sys) user) sys
         (createSystem (cons user (getUser sys)) (getName sys) (getDrives sys) date-now)
        )
    )
)

;Dominio: sys (list) - user (string)
;Recorrido: system (list)
;Descripción: Logea a los usuarios dejandolos con un true
;Tipo de recursion: No empleada
(define login (lambda (sys user) 
    (if (filterElement (getUser sys) user)
        (createSystem (add-login (getUser sys) user) (getName sys) (getDrives sys) date-now)
        sys
        )
    )
)

;Dominio: sys (list)
;Recorrido: sys (list)
;Descripción: Desloguea a los usuarios quitandoles el true
;Tipo de recursion: No empleada
(define logout (lambda (sys)
    (cons (filter string? (getUser sys)) (cdr sys))
    )
)

;-----------------------Otras operaciones-----------------------
;Dominio: lst (list) - element (string)
;Recorrido: System (list)
;Descripcion: Si el element es igual al primer elemento de la lista le agrega un true
;Sino sigue recorriendo la lista
;Tipo de recursion: Natural
(define (add-login lst element)
  (cond [(null? lst) null]
        [(equal? (car lst) element) (append (take lst 1) (list #t) (drop lst 1))]
        [else (cons (first lst) (add-login (rest lst) element))]
        )   
    )

(provide (all-defined-out))