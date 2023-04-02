#lang racket
(provide (all-defined-out))

;Identifica si es que un elemento esta dentro de una lista
(define (filter-element element lst)
  (cond [(null? lst) #f]
        [(equal? element (car lst)) #t]
        [else (filter-element element (cdr lst))]
    )
)

;Identifica si es que un elemento esta dentro de una lista de lista
(define (filter-list element lst)
  (cond [(null? lst) #f]
        [(member element (apply append lst)) #t]
        [else (filter-list element (cdr lst))]
    )
)
;Uno dos distas simulando el append
(define (unir list-1 list-2)
    (if (null? list-1);si la primera lista esta vacia retorna el segundo 
        (list list-2)
        (cons (car list-1) (unir (cdr list-1) list-2)));si la primera lista no es vacia, se hace car y cdrs para concatenar por partes
    )

;Agrega usuarios al sistema
(define (add-user sys  user)
    (if (filter-element user (list-ref sys 0)) sys
         (cons (unir (list-ref sys 0) user) (cdr sys))
        )
)

;Agrega un true al lado de los user para identificar que estan logeados
(define (login sys user) 
    (if (filter-element user (list-ref sys 0)) (cons (unir (list-ref sys 0) #t) (cdr sys))
        (sys)
        )
    )

;Quita los true para que se sepa que no esta logeado
(define (logout sys)
    (cons (filter string? (list-ref sys 0)) (cdr sys))
    )
