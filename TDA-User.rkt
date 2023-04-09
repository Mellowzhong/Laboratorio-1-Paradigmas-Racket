#lang racket
(require "Fecha.rkt")
(require "Main.rkt")

;Dominio: sys (list) - user (string)
;Recorrido: system (list)
;Descripción: Añade un usuario al sistema
;Tipo de recursion: No empleada
(define add-user (lambda (sys  user)
    (if (filter-element user (list-ref sys 0)) sys
         (cons (unir (list-ref sys 0) user) (cdr sys))
        )
    )
)

;Dominio: sys (list) - user (string)
;Recorrido: system (list)
;Descripción: Logea a los usuarios dejandolos con un true
;Tipo de recursion: No empleada
(define login (lambda (sys user) 
    (if (filter-element user (list-ref sys 0))
            (list (add-login (list-ref sys 0) user)
                (list-ref sys 1)
                (list-ref sys 2)
                date-now
                ) 
        (sys)
        )
    )
)
;Dominio: sys (list)
;Recorrido: sys (list)
;Descripción: Desloguea a los usuarios quitandoles el true
;Tipo de recursion: No empleada
(define logout (lambda (sys)
    (cons (filter string? (list-ref sys 0)) (cdr sys))
    )
)

(provide (all-defined-out))