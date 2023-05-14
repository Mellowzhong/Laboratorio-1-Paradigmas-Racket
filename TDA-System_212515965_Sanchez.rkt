#lang racket
(require "TDA_Fecha_212515965_Sanchez.rkt")
(require "TDA_Drives_212515965_Sanchez.rkt")
(require "TDA-User_212515965_Sanchez.rkt")
(require "TDA_Path_212515965_Sanchez.rkt")

;-----------------------Representacion-----------------------
;Se presenta el TDA System, el cual corresponde tal y como indica su nombre a una representacion
;del system, el cual contiene la fecha, el nombre del sistema, los drives, el path y la papelera de reciclaje. Esta representacion
;esta dada por una lista con estos elementos los cuales se ven en el constructor demonimado system y set-system.

;-----------------------Constructor-----------------------
;Descripcion: Funcion que crea un sistema.
;Dom: system-name (string)
;Rec: system (list)
;Tipo de recursión: No empleada
(define (system system-name)
  (list (get-current-date) system-name null null null null)
)

;Descripcion: Funcion que recopila y une los datos del sistema.
;Dom: system=name (string) X system-drives (list) X system-users (list) X system-path (list)
;Rec: system (list)
;Tipo de recursión: No empleada
(define (set-system system-name system-drives system-users system-path system-rb)
  (list (get-current-date) system-name system-drives system-users system-path system-rb)
)

;-----------------------Modificadores-----------------------
;Dom: s-name (string) - s-drives (list) - s-users (list) - s-path (list) - s-rb (list) - d-letter (char) - d-name (string)  - d-capacity (string)
;Rec: updated-system (list)
;Descripcion: Funcion auxiliar de add-drive, se encarga de actualizar el sistema sin antes verificar que la unidad ya no exista.
;Tipo de recursión: No empleada
(define add-drive-aux (lambda (s-name s-drives s-users s-path s-rb) (lambda (d-letter d-name d-capacity)
    (if (not (source-exists? d-letter s-drives))
        (set-system s-name (set-drives-add-drive d-letter d-name d-capacity s-drives) s-users s-path s-rb)
        (set-system s-name s-drives s-users s-path s-rb)
    )
    ))
)

;Dom: outdated-system (list) - letter (char) - name (string) - capacity (int)
;Rec: updated-system (list)
;Descripcion: Funcion que crea una unidad dentro del sistema.
;Tipo de recursión: No empleada
(define add-drive (lambda (outd-system) (lambda (d-letter d-name d-capacity)
    ((add-drive-aux (get-system-name outd-system) (get-system-drives outd-system) 
                    (get-system-users outd-system) (get-system-path outd-system)
                    (get-system-rb outd-system)) 
                    (char-downcase d-letter) (string-downcase d-name) d-capacity)
    ))
)

;Dom: s-name (string) - s-drives (list) - s-users (list) - s-path (list) - s-rb (list) - username (string)
;Rec: updated-system (list)
;Descripcion: Funcion auxiliar de register, se encarga de crear al usuario sin antes verificar
;que no exista otro usuario con el mismo username.
;Tipo de recursión: No empleada
(define register-aux (lambda (s-name s-drives s-users s-path s-rb) (lambda (username)
    (if (not (username-exists? username s-users))
        (set-system s-name s-drives (set-users-system-reg username s-users) s-path s-rb)
        (set-system s-name s-drives s-users s-path s-rb)
    )
    ))
)

;Dom: outdated-system (list) - username (string) 
;Rec: updated-system (list)
;Descripcion: Funcion que añade un nuevo usuario dentro del sistema.
;Tipo de recursión: No empleada
(define register (lambda (outd-system) (lambda (username)
    ((register-aux (get-system-name outd-system) (get-system-drives outd-system) 
                   (get-system-users outd-system) (get-system-path outd-system)
                   (get-system-rb outd-system))
                   (string-downcase username))
    ))
)

;Dom: s-name (string) - s-drives (list) - s-users (list) - s-path (list) - s-rb (list) - username (string)
;Rec: updated-system (list)
;Descripcion: Funcion auxiliar de login, se encarga de loguear al usuario indicado sin antes
;revisar que no exista otro usuario logeado.
;Tipo de recursión: No empleada
(define login-aux (lambda (s-name s-drives s-users s-path s-rb) (lambda (username)
    (if (and (username-exists? username s-users) (not (someone-logged-in? s-users)))
        (set-system s-name s-drives (set-users-system-login username s-users) s-path s-rb)
        (set-system s-name s-drives s-users s-path s-rb)
    )
    ))
)

;Dom: outdated-system (list) X username (string)
;Rec: updated-system (list)
;Descripcion: Funcion que loguea al usuario seleccionado.
;Tipo de recursión: No empleada
(define login (lambda (outd-system) (lambda (username)
    ((login-aux (get-system-name outd-system) (get-system-drives outd-system) 
                (get-system-users outd-system) (get-system-path outd-system)
                (get-system-rb outd-system))
                (string-downcase username))
    ))
)

;Dom: s-name (string) - s-drives (list) - s-users (list) - s-path (list) - s-rb (list)
;Rec: updated-system (list)
;Descripcion: Funcion auxiliar de logout, se encarga de desloguear al usuario en caso de que
;exista uno logueado.
;Tipo de recursión: No empleada
(define logout-aux (lambda (s-name s-drives s-users s-path s-rb)
    (if (someone-logged-in? s-users)
        (set-system s-name s-drives (set-users-system-logout s-users) s-path s-rb)
        (set-system s-name s-drives s-users s-path s-rb)
    )
    )
)

;Dom: outdated-system (list)
;Rec: updated-system (list)
;Descripcion: Funcion que desloguea al usuario.
;Tipo de recursión: No empleada
(define logout (lambda (outd-system)
    (logout-aux (get-system-name outd-system) (get-system-drives outd-system) 
                (get-system-users outd-system) (get-system-path outd-system)
                (get-system-rb outd-system))
    )
)

;Dom: s-name (string) - s-drives (list) - s-users (list) - s-path (list) - s-rb (list) - d-letter (char)
;Rec: updated-system (list)
;Descripcion: Funcion auxiliar de switch-drive, se encarga de cambiar la unidad que esta en
;la ruta del sistema sin antes verificar que la unidad seleccionada exista.
;Tipo de recursión: No empleada
(define switch-drive-aux (lambda (s-name s-drives s-users s-path s-rb) (lambda (d-letter)
    (if (source-exists? d-letter s-drives)
        (set-system s-name s-drives s-users (set-path-sd d-letter) s-rb)
        (set-system s-name s-drives s-users s-path s-rb)
    )
    ))
)

;Descripcion: Funcion que cambia la unidad en la que se quiere trabajar.
;Dom: outdated-system (list) X letter-drive (char)
;Rec: updated-system (list)
;Tipo de recursión: No empleada
(define switch-drive (lambda (outd-system) (lambda (d-letter)
    ((switch-drive-aux (get-system-name outd-system) (get-system-drives outd-system) 
                       (get-system-users outd-system) (get-system-path outd-system)
                       (get-system-rb outd-system))
                       (char-downcase d-letter))
    ))
)

;Dom: s-name (string) - s-drives (list) - s-users (list) - s-path (list) - s-rb (list) - directory-name (string)
;Rec: updated-system
;Descripcion: Funcion auxiliar de md, crea carpetas dentro de la lista de unidades,
;la ubicacion de la carpeta depende de la ruta del sistema.
;Tipo de recursión: No empleada
(define md-aux (lambda (s-name s-drives s-users s-path s-rb) (lambda (directory-name hidden? only-r?)
    (if (someone-logged-in? s-users)
        (set-system s-name (set-drives-rec (list directory-name (get-current-date) 
                                                 (get-logged-user s-users) hidden? only-r?) 
                                           sd-md s-drives s-path) 
                    s-users s-path s-rb)
        (set-system s-name s-drives s-users s-path s-rb)
    )
    ))
)

;Dom: outdated-system (list) - directory-name (string)
;Rec: updated-system
;Descripcion: Funcion que crea carpetas segun la ruta del sistema.
;Tipo de recursión: No empleada
(define md (lambda (outd-system) (lambda (directory-name . args)
    (cond [(null? args)
            ((md-aux (get-system-name outd-system) (get-system-drives outd-system) 
                    (get-system-users outd-system) (get-system-path outd-system)
                    (get-system-rb outd-system))
                    (string-downcase directory-name) #f #f)]
        [(equal? (length args) 1)
            (if (equal? (char-downcase (car args)) #\h)
                ((md-aux (get-system-name outd-system) (get-system-drives outd-system) 
                        (get-system-users outd-system) (get-system-path outd-system)
                        (get-system-rb outd-system))
                        (string-downcase directory-name) #t #f)
                (if (equal? (char-downcase (car args)) #\r)
                    ((md-aux (get-system-name outd-system) (get-system-drives outd-system) 
                            (get-system-users outd-system) (get-system-path outd-system)
                            (get-system-rb outd-system))
                            (string-downcase directory-name) #f #t)
                    ((md-aux (get-system-name outd-system) (get-system-drives outd-system) 
                            (get-system-users outd-system) (get-system-path outd-system)
                            (get-system-rb outd-system))
                            (string-downcase directory-name) #f #f)
                )
            )]
        [(>= (length args) 2)
            ((md-aux (get-system-name outd-system) (get-system-drives outd-system) 
                    (get-system-users outd-system) (get-system-path outd-system)
                    (get-system-rb outd-system))
                    (string-downcase directory-name) #t #t)
        ]

    )
    ))
)

;Dom: s-name (string) - s-drives (list) - s-users (list) - s-path (list) - s-rb (list) - path or directory-name (string)
;Rec: updated-system (list)
;Descripcion: Funcion auxiliar de cd, cambia la ruta segun lo que quiera el usuario siempre
;y cuando la nueva ruta realmente exista.
;Tipo de recursión: No empleada
(define cd-aux (lambda (s-name s-drives s-users s-path s-rb) (lambda (directory-name)
    (if (someone-logged-in? s-users)
        (cond
        [(string=? directory-name "..")
            (set-system s-name s-drives s-users (set-path-return s-path) s-rb)
        ]
        [(string=? directory-name "/")
            (set-system s-name s-drives s-users (set-path-root s-path) s-rb)
        ]
        [(string-is-path? directory-name)
            (if (is-path-a-new-path? directory-name)
                (set-system s-name s-drives s-users (set-path-new-path directory-name s-path s-drives) s-rb)
                (set-system s-name s-drives s-users (set-path-add-path directory-name s-path s-drives) s-rb)
            )
        ]
        [else
            (set-system s-name s-drives s-users (set-path-add-directory directory-name s-path s-drives) s-rb)
        ]
        )
        (set-system s-name s-drives s-users s-path s-rb)
    )
    ))
)

;Dom: outdated-system (list) - path or directory-name (string)
;Rec: updated-system (list)
;Descripcion: Funcion que cambia la direccion de la ruta del sistema.
;Tipo de recursión: No empleada
(define cd (lambda (outd-system) (lambda (directory-name)
    ((cd-aux (get-system-name outd-system) (get-system-drives outd-system) 
             (get-system-users outd-system) (get-system-path outd-system)
             (get-system-rb outd-system))
             (string-downcase directory-name))
    ))
)

;-----------------------Otras operaciones-----------------------
;Descripcion: Funcion que permite ejecutar una funcion sobre el sistema.
;Dom: outdated-system (list) X function (function)
;Rec: updated-system (list)
;Tipo de recursión: No empleada
(define (run outd-system function)
    (curry function outd-system)
)

;Dom: outdated-system (list)
;Rec: system-name (string)
;Descripcion: Funcion que obtiene el nombre del sistema.
;Tipo de recursión: No empleada
(define (get-system-name outd-system)
    (list-ref outd-system 1)
)

;Dom: outdated-system (list)
;Rec: system-drives (list)
;Descripcion: Funcion que obtiene las unidades dentro del sistema.
;Tipo de recursión: No empleada
(define (get-system-drives outd-system)
    (list-ref outd-system 2)
)

;Dom: outdated-system (list)
;Rec: system-users (list)
;Descripcion: Funcion que obtiene los usuarios dentro del sistema.
;Tipo de recursión: No empleada
(define (get-system-users outd-system)
    (list-ref outd-system 3)
)

;Dom: outdated-system (list)
;Rec: system-path (list)
;Descripcion: Funcion que obtiene la ruta dentro del sistema.
;Tipo de recursión: No empleada
(define (get-system-path outd-system)
    (list-ref outd-system 4)
)

;Dom: outdated-system (list)
;Rec: system-recycle-bin (list)
;Descripcion: Funcion que obtiene la papelera de reciclaje dentro del sistema.
;Tipo de recursión: No empleada
(define (get-system-rb outd-system)
    (list-ref outd-system 5)
)

(provide (all-defined-out))