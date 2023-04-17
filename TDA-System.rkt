#lang racket
(require "Fecha.rkt")

;-----------------------Representacion-----------------------
;Se presenta el TDA System, el cual corresponde tal y como indica su nombre a una representacion
;del system, el cual contiene los usuarios, el nombre del system, las unidades y la fecha. Esta representacion
;esta dada por una lista con estos elementos los cuales se ven en el constructor demonimado createSystem

;-----------------------Constructor-----------------------
;Dominio: user (list) - name (string) - units (list) - date-now (string)
;Recorrido: systeam (list)
;Descripción: Crea el sistema con los usuarios nombre los drives (unidades) y la fecha
;Tipo de recursion: No empelada
(define (createSystem user name units date-now)
    (list user 
        name 
        units 
        date-now 
    )
)

;Dominio: drive (string) - name (string) - capacity (integer) - content (list) - select (boolean)
;Recorrido: drive (list)
;Descripción: Crea una lista con el drive, nombre contenido y si esta seleccionado o no
;Tipo de recursion: No empelada
(define (createDrive drive name capacity content select)
    (list drive name capacity content select)
)

;Dominio: name (string) - content (list) - select (boolean)
;Recorrido: folder (list)
;Descripción: Crea una lista con un nombre contenido y si esta seleciconado o no
;Tipo de recursion: No empleada
(define (createFolder name content select)
    (list name content select)
)
;-----------------------Pertenencia-----------------------

;Dominio: element (string) - lst (list)
;Recorrido: boolean
;Descripción: Filtra los elementos dentro de una lista de listas
;Tipo de recursion: Cola
(define (filterList lst element)
    (cond [(null? lst) #f]
        [(list? (car lst))   
         (or (filterList (car lst) element) 
            (filterList (cdr lst) element))]
        [(equal? element (car lst)) #t]  
        [else (filterList (cdr lst) element)]
    )
)

;Dominio: element (string) - lst (list)
;Recorrido: boolean
;Descripción: Filtra los elementos dentro de una lista
;Tipo de recursion: Natural
(define (filterElement lst element)
  (cond [(null? lst) #f]
        [(equal? element (car lst)) #t]
        [else (filterElement (cdr lst) element)]
    )
)
;-----------------------Modificadores-----------------------

;Dominio:  sys(list) - unit-sys(string) - name-sys(string) - capacity-sys(string)
;Recorrido: system (list)
;Descripción: Añade una particion al sistema
;Tipo de recursion: No empleada
(define add-drive (lambda (sys drive name capacity)
        (if (filterList (getDrives sys) drive)
            (createSystem (getUser sys) (getName sys) (getDrives sys) date-now)
            (createSystem (getUser sys) (getName sys) (cons (createDrive drive name capacity null #f) (getDrives sys)) date-now)
        )
    )
)

;Dominio: sys(list) - element(string)
;Recorrido: system(list)
;Descripción: Crea una lista donde se almacenaran las carpetas de la unidad seleccionada
;Tipo de recursion: No empleada
(define switch-drive (lambda (sys drive)
        (if (filterElement (getUser sys) #t)
            (if (filterList (getDrives sys) drive)
                (createSystem (getUser sys) (getName sys) (selectDrive (getDrives sys) drive) date-now)
                (createSystem (getUser sys) (getName sys) (unSelectDrive (getDrives sys) drive) date-now)
                )
            (createSystem (getUser sys) (getName sys) (getDrives sys) date-now)
        )      
    )
)

;Dominio: sys (lst) - element (string)
;Recorrido: system (list)
;Descripción: Agrega una carpeta si este no se encuentra repetida
;Tipo de recursion: No empleada
(define md (lambda (sys element)
        (if (filterList (getDrives sys) element)
            (createSystem (getUser sys) (getName sys) (getDrives sys) date-now)
            (createSystem (getUser sys) (getName sys) (addFolder (getDrives sys) element) date-now)
        )
    )
)

;Dominio: sys (lst) - element (string)
;Recorrido: system (list)
;Descripción: Cambia el directorio donde se agregaran las carpetas
;Tipo de recursion: No empleada
(define cd (lambda (sys element)
        (if (filterList (getDrives sys) element)
            (createSystem (getUser sys) (getName sys) (selectDictory (getDrives sys) element) date-now)
            (createSystem (getUser sys) (getName sys) (getDrives sys) date-now)
        )
    )
)

;-----------------------Otras operaciones-----------------------
;Dominio: sys(list) - function(string)
;Recorrido: function(string) - sys (list)
;Descripción: Recibe el sistema y la funcion a efectuar
;efectua la funcion pasando el sistema como uno de sus parametros
;Tipo de recursion: No empleada
(define run (lambda (sys funcion) 
    (curry funcion sys)
    )
)

;Dominio: name(string)
;Recorrido: system (list)
;Descripción: Recibe un nombre y crea un nuevo sistema con ese nombre
;Tipo de recursion: No empleada
(define system (lambda (name)
        (createSystem null name null null)
    )
)

;Dominio: lst (list)
;Recorrido: users (list)
;Descripción: Entrega la informacion de los usuarios
;Tipo de recursion: No empleada
(define (getUser lst)
    (car lst)
)

;Dominio: lst (list)
;Recorrido: name (string)
;Descripción: Entrega la informacion del nombre del sistema
;Tipo de recursion: No empleada
(define (getName lst)
    (cadr lst)
)

;Dominio: lst (list)
;Recorrido: drives (list)
;Descripción: Entrega la informacion de los drives (unidades) del sistema
;Tipo de recursion: No empleada
(define (getDrives lst)
    (caddr lst)
)

;Dominio: lst (list)
;Recorrido: drive (string)
;Descripción: Entrega la informacion del drive (nombre de la particion)
;Tipo de recursion: No empleada
(define (getDriveUnit lst)
    (car lst)
)

;Dominio: lst (list)
;Recorrido: name (string)
;Descripción: Entrega la informacion del nombre del drive
;Tipo de recursion: No empleada
(define (getNameUnit lst)
    (cadr lst)
)

;Dominio: lst (list)
;Recorrido: capacity (integer)
;Descripción: Entrega la informacion de la capacidad del drive
;Tipo de recursion: No empleada
(define (getCapacityUnit lst)
    (caddr lst)
)

;Dominio: lst (list)
;Recorrido: Content (list)
;Descripción: Entrega la informacion del contenido de los drives
;Tipo de recursion: No empleada
(define (getContentUnit lst)
    (cadddr lst)
)

;Dominio: lst (list)
;Recorrido: result (boolean)
;Descripción: Entrega la informacion de si la carpeta o drive esta seleccionado
;En caso de estar vacio entrega #f
;Tipo de recursion: No empleada
(define (getSelect lst)
(if (null? lst) #f
    (last lst))
)

;Dominio:lst (list) - element (string)
;Recorrido: System (list)
;Descripción: Cuando el Drive sea el mismo que el entregado se agrega y se queda como seleccionado (#t)
;Tipo de recursion: Natural
(define (selectDrive lst element)
    (cond [(null? lst) null] ;Condicion base
        [(equal? element (getDriveUnit (car lst)))
            (cons (createDrive (getDriveUnit (car lst)) ;Se agrega
                (getNameUnit (car lst)) 
                (getCapacityUnit (car lst))
                (getContentUnit (car lst)) #t) 
                (selectDrive (cdr lst) element))]
        [else (cons (car lst) (selectDrive (cdr lst) element))] ;Copntinua buscando
    )
)

;Dominio:lst (list) - element (string)
;Recorrido: System (list)
;Descripción: Cuando el Drive no sea el mismo se agrega y queda como no seleccionado (#f)
;Tipo de recursion: Natural
(define (unSelectDrive lst element)
    (cond [(null? lst) null]
        [(equal? element (getDriveUnit (car lst)))
            (cons (createDrive (getDriveUnit (car lst)) 
                (getNameUnit (car lst)) 
                (getCapacityUnit (car lst))
                (getContentUnit (car lst)) #f) 
                (selectDrive (cdr lst) element))]
        [else (cons (car lst) (selectDrive (cdr lst) element))]
    )
)

;Dominio: lst (list) - element (string)
;Recorrido: System (list)
;Descripcion: Cuando el drive esta seleccionado y ademas cuando no hay ninguna carpeta seleccionada
;Se agrega una carpeta al contenido del drive
;Tipo de recursion: Natural
(define (addFolder lst element)
    (cond [(null? (car lst)) null]
        [else(if (equal? (getSelect (car lst)) #t)
                (if (equal? (getSelect (getContentUnit (car lst))) #t)
                    (createDrive (getDriveUnit (car lst))
                        (getNameUnit (car lst)) 
                        (getCapacityUnit (car lst)) 
                        (append (list (createFolder (getNameFolder (car lst)) 
                                    (getContentUnitFolder (car lst)) #f)
                                    (getSelect (car lst))) 
                                    (getContentUnit (car lst)))
                        (getSelect (car lst)))
                    (createDrive (getDriveUnit (car lst))
                        (getNameUnit (car lst)) 
                        (getCapacityUnit (car lst)) 
                        (append (list (createFolder element null #f)) (getContentUnit (car lst)))
                        (getSelect (car lst)))
                )
                (append (list (car lst)) (list (addFolder (cdr lst) element)))
            )
        ]
    )
)

;Dominio: lst (list) - element (string)
;Recorrido: System (list)
;Descripcion: Cuando el drive esta seleccionado busca la carpeta la cual se desea seleccionar entre
;el contenido del drive
;Tipo de recursion: Natural
(define (selectDictory lst element)
    (cond [(null? (car lst)) null]
        [else(if (equal? (getSelect (car lst)) #t)
                (if (equal? (getNameFolder (car (getContentUnit (car lst)))) element)
                    (createDrive (getDriveUnit (car lst))
                        (getNameUnit (car lst))
                        (getCapacityUnit (car lst))
                        (append (list (createFolder (getNameFolder (car (getContentUnit (car lst)))) null #t)) 
                                (list (getCapacityUnit (getContentUnit (car lst)))))
                        (getSelect (car lst))) 
                    (createDrive (getDriveUnit (car lst))
                        (getNameUnit (car lst))
                        (getCapacityUnit (car lst))
                        (append (list (createFolder (getNameFolder (car (getContentUnit (car lst)))) null #f)) 
                                (list (getCapacityUnit (getContentUnit (car lst)))))
                        (getSelect (car lst))))
                (append (list (car lst)) (list (selectDictory (cdr lst) element)))
            )
        ]
    )
)

;Dominio: lst (list)
;Recorrido: result (string)
;Descripción: Entrega la informacion del nombre de la carpeta
;Tipo de recursion: No empleada
(define (getNameFolder lst)
    (car lst)
)

;Dominio: lst (list)
;Recorrido: result (lst)
;Descripción: Entrega la informacion del contenido de la carpeta
;Tipo de recursion: No empleada
(define (getContentUnitFolder lst)
    (cadr lst)
)

(provide (all-defined-out))