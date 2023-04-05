#lang racket

;Dominio: element (string) - lst (list)
;Recorrido: boolean
;Descripción: Filtra los elementos dentro de una lista
(define (filter-element element lst)
  (cond [(null? lst) #f]
        [(equal? element (car lst)) #t]
        [else (filter-element element (cdr lst))]
    )
)

;Dominio: element (string) - lst (list)
;Recorrido: boolean
;Descripción: Filtra los elementos dentro de una lista de listas
(define (filter-list element lst)
  (cond [(null? lst) #f]
        [(member element (apply append lst)) #t]
        [else (filter-list element (cdr lst))]
    )
)

;Dominio: list-1 (list) - list-2 (list)
;Recorrido: list
;Descripción: Une 2 listas 
(define (unir list-1 list-2)
    (if (null? list-1);si la primera lista esta vacia retorna el segundo 
        (list list-2)
        (cons (car list-1) (unir (cdr list-1) list-2)));si la primera lista no es vacia, se hace car y cdrs para concatenar por partes
    )

;Dominio: sys (list) - user (string)
;Recorrido: system (list)
;Descripción: Añade un usuario al sistema
(define (add-user sys  user)
    (if (filter-element user (list-ref sys 0)) sys
         (cons (unir (list-ref sys 0) user) (cdr sys))
        )
)

;Dominio: sys (list) - user (string)
;Recorrido: system (list)
;Descripción: Logea a los usuarios dejandolos con un true
(define (login sys user) 
    (if (filter-element user (list-ref sys 0)) (cons (unir (list-ref sys 0) #t) (cdr sys))
        (sys)
        )
    )
    
;Dominio: sys (list)
;Recorrido: sys (list)
;Descripción: Desloguea a los usuarios quitandoles el true
(define (logout sys)
    (cons (filter string? (list-ref sys 0)) (cdr sys))
    )

(provide (all-defined-out))