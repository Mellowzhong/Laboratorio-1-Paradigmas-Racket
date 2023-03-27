#lang racket

;Crea un nuevo sistema 
(define (system new-system . content-system)
 (list new-system content-system))

;recibe como argumento system y funcion, posteriormente ejecuta la funcion entregada pasando
;como uno de sus argumentos el system y los que se entregan fuera del llamado
(define (run system funcion) 
    (curry funcion system))

;Agrega una nueva particion al sistema
(define (add-drive sys unidad nombre capacidad) 
    (system (car sys) (list unidad nombre capacidad)))