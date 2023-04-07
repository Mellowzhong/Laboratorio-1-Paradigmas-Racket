#lang racket
(require "Fecha.rkt")
(require "TDA-User.rkt")

;Dominio:lst(list) - element(string)
;Recorrido: result(list)
;Descripción: Agarra la lista entregada y cuando el elemento sea igual al de la lista le agrega una lista 
;sino simplemente lo agrega a la lista sin la lista 
(define (add-folder lst element)
  (cond ((empty? lst) (list element))
        ((equal? (car lst) element) (append (take lst 3) (list (list (string-append "Folder-" (string element)))) (drop lst 3)))
        (else (cons (first lst) (add-folder (rest lst) element)))))

;Dominio: name(string)
;Recorrido: system (list)
;Descripción: Recibe un nombre y crea un nuevo sistema con ese nombre
(define (system name) 
         (list (list);user
                name
                (list);unit
                date-now
           )
    )

;Dominio: sys(list) - function(string)
;Recorrido: function(string) - sys (list)
;Descripción: Recibe el sistema y la funcion a efectuar
;efectua la funcion pasando el sistema como uno de sus parametros
(define (run sys funcion) 
    (curry funcion sys)
    )

;Dominio:  sys(list) - unit-sys(string) - name-sys(string) - capacity-sys(string)
;Recorrido: system (list)
;Descripción: Añade una particion al sistema
(define (add-drive sys unit-sys name-sys capacity-sys) 
        (list (list-ref sys 0)
        (list-ref sys 1) 
        (if (filter-list unit-sys (list-ref sys 2)) (list-ref sys 2)
          (unir (list-ref sys 2) (list unit-sys name-sys capacity-sys))
            )
        date-now)
    )

;Dominio: sys(list) - element(string)
;Recorrido: system(list)
;Descripción: Crea una lista donde se almacenaran las carpetas de la unidad seleccionada
(define (switch-drive sys element)
    (if (filter-element #t (list-ref sys 0))
        (if (filter-element element (list-ref sys 2))
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

;Dominio: sys(list) - element(string)
;Recorrido: system(list)
;Descripción: Crea un directorio en la carpeta seleccionada anteriormente
(define (md sys element)
    (if (filter-element element (list-ref sys 4)) sys
        (unir (remove (last sys) sys) (append (last sys) (list  element )))
        )
    )

;Dominio: sys(list) - element(string)
;Recorrido: system(list)
;Descripción: Agregara una lista vacia en el directorio donde se realizara el cambio
(define (cd sys element)
    (unir (unir(unir(unir (list(list-ref sys 0)) 
        (list-ref sys 1)) 
            (list-ref sys 2)) 
                (list-ref sys 3))
                    (add-folder element (list-ref sys 4))
    )
)
(provide (all-defined-out))
