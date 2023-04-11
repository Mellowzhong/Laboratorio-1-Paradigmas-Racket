#lang racket
(require "Fecha.rkt")
(require "Main.rkt")

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

;Dominio: sys(list) - function(string)
;Recorrido: function(string) - sys (list)
;Descripción: Recibe el sistema y la funcion a efectuar
;efectua la funcion pasando el sistema como uno de sus parametros
;Tipo de recursion: No empleada
(define run (lambda (sys funcion) 
    (curry funcion sys)
    )
)

;Dominio:  sys(list) - unit-sys(string) - name-sys(string) - capacity-sys(string)
;Recorrido: system (list)
;Descripción: Añade una particion al sistema
;Tipo de recursion: No empleada
(define add-drive (lambda (sys unit-sys name-sys capacity-sys) 
        (list (list-ref sys 0)
        (list-ref sys 1) 
        (if (filter-list unit-sys (list-ref sys 2)) (list-ref sys 2)
          (append (list-ref sys 2) (list unit-sys name-sys capacity-sys (list #f)))
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

(define cd (lambda (sys element)
    (list (list-ref sys 0)
        (list-ref sys 1)
        (cond  [(filter-list element (list-ref sys 2))
            (select-dictory element (list-ref sys 2))]
            [else (list-ref sys 2)]
            )
        date-now
        )
    )
)
(provide (all-defined-out))
