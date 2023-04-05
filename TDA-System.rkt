#lang racket
(require "Fecha.rkt")
(require "TDA-User.rkt")

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
    (if (filter-element #t (list-ref sys 0));Inspecciona que haya un login 
        (if (filter-list element (list-ref sys 2)) (unir sys (list (string-append "Folder-" (string element))))
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

;Dominio: element(string) - lst(list)
;Recorrido: result(list)
;Descripción: Agarra la lista entregada y cuando el elemento sea igual al de la lista le agrega una lista vacia
;sino simplemente lo agrega a la lista sin la lista vacia
(define (add-folder element lst)
    (define (tail result lst)
        (cond[(null? lst) (reverse result)]
            [(equal? element (car lst))
                (tail (cons (unir (list(car lst)) (list) ) result) (cdr lst))]
            [else (tail (cons (car lst) result) (cdr lst))]
            )
        )
    (tail '() lst)
    )

;#\C/""folder5"/"folder2"/"hola"
;(md (add-folder ("hola") (add-folder ""folder2") (add-folder "folder5" (add-folder #\C (list-ref sys 2)))))

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
