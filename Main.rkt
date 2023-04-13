#lang racket


;Dominio: element (string) - lst (list)
;Recorrido: list
;Descripción: Añade el elemento 
;Tipo de recursion: Natural
(define (select-dictory element lst)
  (cond [(null? lst) null]
        [(list? (car lst)) 
            (append (function-aux (car lst) element)(rest lst))]
        [else (cons (car lst) (select-dictory element (cdr lst)))]
    )
)

;Dominio: lst (list) - element (string)
;Recorrido: list
;Descripción: Hace la parte de agregar el elemento deseado
;Tipo de recursion: Cola
(define (function-aux lst element)
    (define (tail result lst)
        (cond [(null? lst) (list (reverse result))]
            [(Comprobation lst)
                 (tail result (append (list "no-select") (rest lst)))]
            [(equal? (car lst) element)
                 (tail (cons (list "select" element) result) (cdr lst))]
            [else (tail (cons (car lst) result) (cdr lst))]
        )
    )
    (tail null lst)
)

;Dominio: list-1 (list) - list-2 (list)
;Recorrido: list
;Descripción: Une 2 listas 
;Tipo de recursion: Natural
(define (unir list-1 list-2)
    (if (null? list-1);si la primera lista esta vacia retorna el segundo 
        (list list-2)
        (cons (car list-1) (unir (cdr list-1) list-2)));si la primera lista no es vacia, se hace car y cdrs para concatenar por partes
    )

(define (getUnit lst)
    (car lst)
)
;Dominio:lst (list) - element (string)
;Recorrido: System (list)
;Descripción: Agarra la lista entregada y cuando el elemento sea igual al de la lista le agrega una lista 
;sino simplemente lo agrega a la lista sin la lista 
;Tipo de recursion: Natural
(define (add-folder lst element)
  (cond [(null? lst) null]
        [(equal? (getUnit (car lst)) element) 
            (append 
                (list (reverse (cons (list "select") (reverse (car lst)))))
                (rest lst))]
        [else (cons (first lst) (add-folder (rest lst) element))]
        )
    )

(define (Comprobation lst)
    (if (equal? (last lst) "select") #t
        #f
    )
)
(define (add-element lst element)
    (cons element (last lst))
)
;Dominio: lst (list) - element (string)
;Recorrido: System (list)
;Descripcion: Busca cuando es una lista y si es el primer elemto es true agrega la carpeta nueva
;Sino sigue buscando en la lista
;Tipo de recursion: Natural
(define (add-directory lst element)
  (cond [(null? lst) null]
        [(list? (car lst)) 
            (if (Comprobation (last (car lst))) 
                (append (list (append (take (car lst) 3) (list (add-element (car lst) element)))) (rest lst))
                (cons (add-directory (car lst) element) (cdr lst)))]
        [else (cons (car lst) (add-directory (cdr lst) element))]
        )
    )
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
    
;Dominio: element (string) - lst (list)
;Recorrido: boolean
;Descripción: Filtra los elementos dentro de una lista
;Tipo de recursion: Natural
(define (filter-element element lst)
  (cond [(null? lst) #f]
        [(equal? element (car lst)) #t]
        [else (filter-element element (cdr lst))]
    )
)

;Dominio: element (string) - lst (list)
;Recorrido: boolean
;Descripción: Filtra los elementos dentro de una lista de listas
;Tipo de recursion: Cola
(define (filter-list elem lista)
    (cond [(null? lista) #f]
        [(list? (car lista))   
         (or (filter-list elem (car lista)) 
            (filter-list elem (cdr lista)))]
        [(equal? elem (car lista)) #t]  
        [else (filter-list elem (cdr lista))]
    )
)

;Mensaje pa nico

(provide (all-defined-out))