#lang racket
(provide (all-defined-out))

(define (filter-element element lst)
  (cond [(null? lst) #f]
        [(equal? element (car lst)) #t]
        [else (filter-element element (cdr lst))]
    )
)

(define (unir list-1 list-2)
    (if (null? list-1);si la primera lista esta vacia retorno el segundo 
        (list list-2)
        (cons (car list-1) (unir (cdr list-1) list-2)));si la primera lista no es vacia, hago car y cdrs para concatenar por partes
    )

(define (add-user sys  user)
    (if (filter-element user (list-ref sys 0)) sys
         (cons (unir (list-ref sys 0) user) (cdr sys))
        )
)


