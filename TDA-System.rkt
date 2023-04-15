#lang racket
(require "Fecha.rkt")

;-----------------------Constructor-----------------------
;Dominio: name(string)
;Recorrido: system (list)
;Descripción: Recibe un nombre y crea un nuevo sistema con ese nombre
;Tipo de recursion: No empleada
(define system (lambda (name) 
         (list (list);user
                name
                (list);unit
                date-now
           )
    )
)
;--------------------------------------------------------
;-----------------------Modificadores-----------------------
;Dominio:  sys(list) - unit-sys(string) - name-sys(string) - capacity-sys(string)
;Recorrido: system (list)
;Descripción: Añade una particion al sistema
;Tipo de recursion: No empleada
(define add-drive (lambda (sys unit-sys name-sys capacity-sys) 
        (list (list-ref sys 0)
        (list-ref sys 1) 
        (if (filter-list unit-sys (list-ref sys 2)) (list-ref sys 2)
          (unir (list-ref sys 2) (list unit-sys name-sys capacity-sys))
            )
        date-now)
    )
)

;Dominio: sys(list) - element(string)
;Recorrido: system(list)
;Descripción: Crea una lista donde se almacenaran las carpetas de la unidad seleccionada
;Tipo de recursion: No empleada
(define switch-drive (lambda (sys element)
    (if (filter-element #t (list-ref sys 0))
        (if (filter-list element (list-ref sys 2))
            (list (list-ref sys 0)
                (list-ref sys 1)
                (add-folder (list-ref sys 2) element)
                date-now
                )
                sys
            )
            sys
          )
    )
)
    

(define md (lambda (sys element)
    (list (list-ref sys 0)
        (list-ref sys 1)
        (if (filter-list element (list-ref sys 2))
            (list-ref sys 2)
            (add-directory (list-ref sys 2) element))
        date-now
        )
    )
)

(define (add-element lst element)
    (cons element (last lst))
)
;--------------------------------------------------------
;-----------------------Pertenencia-----------------------
(define (Comprobation lst)
    (if (equal? (last lst) "select") #t
        #f
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
;--------------------------------------------------------
;-----------------------Otras operaciones-----------------------
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

;Dominio:lst (list) - element (string)
;Recorrido: System (list)
;Descripción: Agarra la lista entregada y cuando el elemento sea igual al de la lista le agrega una lista 
;sino simplemente lo agrega a la lista sin la lista 
;Tipo de recursion: Natural
(define (add-folder lst element)
  (cond [(null? lst) null]
        [(equal? (car (car lst)) element) 
            (append 
                (list (reverse (cons (list "select") (reverse (car lst)))))
                (rest lst))]
        [else (cons (car lst) (add-folder (cdr lst) element))]
        )
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
;Dominio: sys(list) - function(string)
;Recorrido: function(string) - sys (list)
;Descripción: Recibe el sistema y la funcion a efectuar
;efectua la funcion pasando el sistema como uno de sus parametros
;Tipo de recursion: No empleada
(define run (lambda (sys funcion) 
    (curry funcion sys)
    )
)
(provide (all-defined-out))
