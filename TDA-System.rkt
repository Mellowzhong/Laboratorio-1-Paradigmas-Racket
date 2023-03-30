#lang racket
(require "Fecha.rkt") ;Se añade el requerimiento para sacar la fecha
(require "TDA_User.rkt")

;Crea un nuevo sistema 
(define (system new-system) 
         (list (list "Usuarios:")
                new-system 
                (list "Unidades:")
                fecha
           )
    )

;recibe como argumento system y funcion, posteriormente ejecuta la funcion entregada pasando
;como uno de sus argumentos el system y los que se entregan fuera del llamado
(define (run sys funcion) 
    (curry funcion sys)
    )

;Agrega una particion al sistema
(define (add-drive sys unidad nombre capacidad) 
        (list (list-ref sys 0)
        (list-ref sys 1) 
        (if (filter-element unidad (list-ref sys 2)) (list unidad nombre capacidad)
         (append (list-ref sys 2) (list unidad nombre capacidad))
            )
        fecha)
    )