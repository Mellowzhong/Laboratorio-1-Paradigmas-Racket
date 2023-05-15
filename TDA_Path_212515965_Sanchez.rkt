#lang racket
(require "TDA_Drives_212515965_Sanchez.rkt")

;-----------------------Representacion-----------------------
;Se presenta el TDA Path, el cual corresponde a tal y como indica su nombre a una representacion del path,
;el cual es la direccion hacia donde uno se debe mover dentro del sistema. Esta respresentación esta dada
;por una serie de funciones las cuales permitiran conocer las rutas por donde se debera mover en el 
;sistema pasando de un string (ruta dada por el usuario) a una lista para poder indendtidicar
;lo anterior mencionado.

;-----------------------Pertenencia-----------------------
;Dom: actual-path (list) - actual-directory (list)
;Rec: boolean
;Descripcion: Funcion recursiva auxiliar que identifica si la ruta pasada existe en el sistema.
;Tipo de recursión: cola
(define (path-exists?-aux current-path current-directory)
    (if (null? current-path)
        #t
        (if (source-exists? (car current-path) current-directory)
            (if (char? (car current-path))
                (path-exists?-aux (cdr current-path) (get-selected-directory-drive (car current-path) 
                                  current-directory))
                (path-exists?-aux (cdr current-path) (get-selected-directory-directory (car current-path)
                                  current-directory))
            )
            #f
        )
    )
)

;Dom: path (list) - drives-system (list)
;Rec: boolean
;Descripcion: Funcion que identifica si la ruta entregada existe en el sistema
;Tipo de recursión: No empleada
(define (path-exists? path drives-system)
    (path-exists?-aux path drives-system)
)

;Dom: foldername (list)
;Rec: boolean
;Descripcion: Funcion recursiva auxiliar que busca el char '/'  para identificar si el foldername es una ruta.
;Tipo de recursión: cola
(define (string-is-path?-aux string-list)
    (if (null? string-list)
        #f
        (if (equal? (car string-list) #\/)
            #t
            (string-is-path?-aux (cdr string-list))
        )
    )
)

;Dom: foldername (string)
;Rec: boolean
;Descripcion: Funcion que busca que el foldername entregado es una ruta.
;Tipo de recursión: No empleada
(define (string-is-path? string)
    (string-is-path?-aux (string->list string))
)
;-----------------------Modificadores-----------------------
;Dom: drive-letter (char)
;Rec: system-path (list)
;Descripcion: Funcion que actualiza la ruta en caso de que se quiera trabajar en una de las unidades.
;Tipo de recursión: No empleada
(define (set-path-sd drive-letter)
    (list drive-letter)
)

;Dom: system-path (list)
;Rec: system-path (list)
;Descripcion: Funcion que actualiza la ruta eliminando la ultima direccion de carpeta.
;Tipo de recursión: No empleada
(define (set-path-return system-path)
    (if (> (length system-path) 1)
        (reverse (cdr (reverse system-path)))
        system-path
    )
)

;Dom: system-path (list)
;Rec: system-path (list)
;Descripcion: Funcion que actualiza la ruta eliminando todas las direcciones excepto la unidad.
;Tipo de recursión: No empleada
(define (set-path-root system-path)
    (if (> (length system-path) 1)
        (list (car system-path))
        system-path
    )
)

;Dom: path-system (list)
;Rec: path-system (list)
;Descripcion: Funcion que actualiza la ruta añadiendo una direccion de carpeta.
;Tipo de recursión: No empleada
(define (set-path-add-directory directory-name system-path system-drives)
    (if (path-exists? (append system-path (list directory-name)) system-drives)
        (append system-path (list directory-name))
        system-path
    )
)

;Dom: path-system (list) - path-directory (list)
;Rec: path-system (list)
;Descripcion: Funcion auxiliar de set-path-add-path, actualiza la ruta del sistema
;agregandole nuevas direcciones.
;Tipo de recursión: cola
(define (set-path-add-path-aux directory-path system-path)
    (if (null? directory-path) 
        system-path
        (set-path-add-path-aux (cdr directory-path) (append system-path (list (car directory-path))))
    )
)

;Dom: directory-path (string) - system-path (list) - system-drives (list)
;Rec: system-path (list)
;Descripcion: Funcion que convierte el string en una ruta adherida a la ruta actual del sistema.
;Tipo de recursión: No empleada
(define (set-path-add-path directory-path system-path system-drives)
    (if (path-exists? (set-path-add-path-aux (string-path->list-path directory-path) system-path) system-drives)
        (set-path-add-path-aux (string-path->list-path directory-path) system-path)
        system-path    
    )
)

;Dom: directory-path (string) - system-path (list) - system-drives (list)
;Rec: new-system-path
;Descipcion: Funcion que cambia la ruta del sistema por una nueva siempre y cuando esta exista.
;Tipo de recursión: No empleada
(define (set-path-new-path directory-path system-path system-drives)
    (if (path-exists? (new-string-path->list-path directory-path) system-drives)
        (new-string-path->list-path directory-path)
        system-path
    )
)

;Dom: directory-path (string) - system-path (list) - system-drives (list)
;Rec: directory-name (list)
;Descipcion: Funcion que recopila la ruta dada cuando se usa la funcion rd y se da una ruta
;que empieza desde el origen.
;Tipo de recursión: No empleada
(define (set-path-new-path-rd directory-path system-path system-drives)
    (reverse (cdr (reverse (set-path-new-path directory-path system-path system-drives))))
)

;Dom: directory-path (string) - system-path (list) - system-drives (list)
;Rec: new-directory-path (list)
;Descipcion: Funcion que recopila la ruta dada cuando se usa la funcion rd y se da
;una ruta que se adhiere a la ruta del sistema.
;Tipo de recursión: No empleada
(define (set-path-add-path-rd directory-path system-path system-drives)
    (reverse (cdr (reverse (set-path-add-path directory-path system-path system-drives))))
)

;-----------------------Otras operaciones-----------------------
;Dom: list-path (list)
;Rec: boolean
;Descripcion: Funcion recursiva auxiliar que busca el char ':' para identificar si la ruta
;entregada comienza desde el origen.
;Tipo de recursión: cola
(define (is-path-a-new-path?-aux path-l)
    (if (null? path-l)
        #f
        (if (equal? (car path-l) #\:)
            #t
            (is-path-a-new-path?-aux (cdr path-l))
        )
    )
)

;Dom: path (string)
;Rec: boolean
;Descripcion: Funcion que identifica si el string de ruta entregado es uno que se mueve con la ruta
;actual o comienza desde un origen.
;Tipo de recursión: No empleada
(define (is-path-a-new-path? path)
    (is-path-a-new-path?-aux (string->list path))
)

;Dom: list-char-path (list) - list-char (list) - list-path (list)
;Rec: list-path (list)
;Descripcion: Funcion recursiva auxiliar que separa las direcciones de una ruta y las almacena en una lista.
;Tipo de recursión: cola
(define (string-path->list-path-aux path l-char l-path)
    (if (null? path)
        (append l-path (list (list->string l-char)))
        (if (equal? (car path) #\/)
            (string-path->list-path-aux (cdr path) null (append l-path (list (list->string l-char))))
            (string-path->list-path-aux (cdr path) (append l-char (list (car path))) l-path)
        )
    )
)

;Dom: path (string)
;Rec: list-path (list)
;Descripcion: Funcion que convierte el string de una ruta en una lista con todas las direcciones.
;Tipo de recursión: No empleada
(define (string-path->list-path path)
    (string-path->list-path-aux (string->list path) null null)
)

;Dom: path-char (list) - l-char (list) - l-path (list)
;Rec: l-path (list)
;Descripcion: Funcion auxiliar que recopila los elementos de una ruta hecha string que va desde el origen.
;Tipo de recursión: cola
(define (new-string-path->list-path-aux path l-char l-path)
    (if (null? path)
        l-path
        (cond
        [(equal? (car path) #\:)
            (new-string-path->list-path-aux (cdr (cdr path)) null (append l-path l-char))
        ]
        [(equal? (car path) #\/)
            (new-string-path->list-path-aux (cdr path) null 
                                            (append l-path (list (list->string l-char))))
        ]
        [else
            (new-string-path->list-path-aux (cdr path) (append l-char (list (car path))) l-path)
        ]
        )
    )
)

;Dom: path (string)
;Rec: path (list)
;Descripcion: Funcion que convierte una ruta que va desde el origen 
;que esta en forma de string en una lista con cada direccion.
;Tipo de recursión: No empleada
(define (new-string-path->list-path path)
    (new-string-path->list-path-aux (string->list path) null null)
)

;Dom: s-path (list) X t-path (list)
;Rec: boolean
;Descipcion: Funcion que identifica si la ruta del sistema esta dentro de la ruta objetivo
;Tipo de recursión: cola
(define (s-path-is-out-t-path? s-path t-path)
    (if (null? s-path)
        #t
        (if (null? t-path)
        #f
            (if (and (char? (car s-path)) (char? (car t-path)))
                (if (equal? (car s-path) (car t-path))
                    (s-path-is-out-t-path? (cdr s-path) (cdr t-path))
                    #t
                )
                (if (string=? (car s-path) (car t-path))
                    (s-path-is-out-t-path? (cdr s-path) (cdr t-path))
                    #t
                )
            )
        )
    
    )
)

;Dom: directory-path (string) - directory-path (list) - directory-drives (list)
;Rec: directory-name (list)
;Descipcion: Funcion que obtiene el nombre de la carpeta que se quiere eliminar y esta dentro
;de la ruta que se adhiere a la ruta del sistema.
;Tipo de recursión: No empleada
(define (get-f-directory-from-sp-new-path directory-path system-path system-drives)
    (car (reverse (set-path-new-path directory-path system-path system-drives)))
)

;Dom: directory-path (string) - system-path (list) - system-drives (list)
;Rec: directory-name (string)
;Descipcion: Funcion que consigue el nombre de la carpeta que se quiere eliminar y esta metido
;dentro de la ruta que comienza desde el origen.
;Tipo de recursión: No empleada
(define (get-f-directory-from-sp-add-path directory-path system-path system-drives)
    (car (reverse (set-path-add-path directory-path system-path system-drives)))
)

(provide (all-defined-out))