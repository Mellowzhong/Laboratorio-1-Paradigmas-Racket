#lang racket
(require racket/date) ;Se añade el requerimiento para sacar la fecha

;Se añade esta funcion, lo que hace es sacar el año, mes y dia y dejarlos en una lista
(define fecha-lista
  (list (date-year (current-date))
        (date-month (current-date))
        (date-day (current-date))))

;Esta funcion lo que hace es concatenar los elementos de la lista y hacerlos un string
(define fecha (string-append (number->string(list-ref fecha-lista 0)) "/"
                            (number->string(list-ref fecha-lista 1)) "/"
                            (number->string(list-ref fecha-lista 2))))

;Crea un nuevo sistema 
(define (system new-system . content-system)
 (list fecha new-system content-system))

;recibe como argumento system y funcion, posteriormente ejecuta la funcion entregada pasando
;como uno de sus argumentos el system y los que se entregan fuera del llamado
(define (run system funcion) 
    (curry funcion system))

;Hace la funcion del append
(define (unir x y)
    (if (null? x);si la primera lista esta vacia retorna la segunda lista 
        (list y)
        (cons (car x) (unir (cdr x) y));si la primera lista no es vacia, se hace el car y cdrs para concatenar por partes
    ))

;Agrega una nueva particion al sistema
(define add-drive (lambda (sys unidad nombre capacidad) 
    (list fecha
    (list-ref sys 1) 
    (unir (list-ref sys 2) (list unidad nombre capacidad)))))