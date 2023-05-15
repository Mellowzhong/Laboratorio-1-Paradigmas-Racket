#lang racket
(require "TDA_Fecha_212515965_Sanchez.rkt")
(require "TDA_Drives_212515965_Sanchez.rkt")
(require "TDA-User_212515965_Sanchez.rkt")
(require "TDA_Path_212515965_Sanchez.rkt")
(require "TDA_Recycle-Bin_212515965_Sanchez.rkt")

;-----------------------Representacion-----------------------
;Se presenta el TDA System, el cual corresponde tal y como indica su nombre a una representacion
;del system, el cual contiene la fecha, el nombre del sistema, los drives, el path y la papelera de reciclaje. Esta representacion
;esta dada por una lista con estos elementos los cuales se ven en el constructor demonimado system y set-system.

;-----------------------Constructor-----------------------
;Dom: system-name (string)
;Rec: system (list)
;Descripcion: Funcion que crea un sistema.
;Tipo de recursión: No empleada
(define (system system-name)
  (list (get-current-date) system-name null null null null)
)

;Dom: system=name (string) X system-drives (list) X system-users (list) X system-path (list)
;Rec: system (list)
;Descripcion: Funcion que recopila y une los datos del sistema.
;Tipo de recursión: No empleada
(define (set-system system-name system-drives system-users system-path system-rb)
  (list (get-current-date) system-name system-drives system-users system-path system-rb)
)

;Dom: name-file (string) - type-file (string) - content-file (string) - hidden? (char or null) - only-read? (char or null)
;Rec: file (list)
;Descripcion: Funcion que crea un archivo.
;Tipo de recursión: No empleada
(define (file name-file type-file content-file . args)
    (cond [(null? args)
            (list (string-downcase name-file) (string-downcase type-file) (string-downcase content-file) #f #f)]
        [(equal? (length args) 1)
            (if (equal? (car args) #\h)
                (list (string-downcase name-file) (string-downcase type-file) (string-downcase content-file) #t #f)
                (list (string-downcase name-file) (string-downcase type-file) (string-downcase content-file) #f #t))]
        [else (list (string-downcase name-file) (string-downcase type-file) (string-downcase content-file) #t #t)]
    )
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
    (if (and (source-exists? d-letter s-drives) (someone-logged-in? s-users))
        (set-system s-name s-drives s-users (set-path-sd d-letter) s-rb)
        (set-system s-name s-drives s-users s-path s-rb)
    )
    ))
)

;Dom: outdated-system (list) - letter-drive (char)
;Rec: updated-system (list)
;Descripcion: Funcion que cambia la unidad en la que se quiere trabajar.
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

;Dom: s-name (string) - s-drives (list) - s-users (list) - s-path (list) - s-rb (list) - d-letter (char) - new-d-name (char)
;Rec: updated-system (list)
;Descripcion: Funcion auxiliar de format, actualiza la lista de unidades en el caso
;de que exista un usuario logueado.
;Tipo de recursión: No empleada
(define format-aux (lambda (s-name s-drives s-users s-path s-rb) (lambda (d-letter new-d-name)
    (if (and (someone-logged-in? s-users) (s-path-is-out-t-path? s-path (list d-letter)))
        (set-system s-name (set-drives-format d-letter new-d-name s-drives) s-users s-path s-rb)
        (set-system s-name s-drives s-users s-path s-rb)
    )
    ))
)

;Dom: outdated-system (list) - d-letter (char) - new-d-name (string)
;Rec: updated-system (list)
;Descripcion: Funcion que busca formatear una unidad dentro del sistema.
;Tipo de recursión: No empleada
(define format (lambda (outd-system) (lambda (d-letter new-d-name)
    ((format-aux (get-system-name outd-system) (get-system-drives outd-system)
                 (get-system-users outd-system) (get-system-path outd-system)
                 (get-system-rb outd-system)) 
                 (char-downcase d-letter) (string-downcase new-d-name))
    ))
)

;Dom: s-name (string) - s-drives (list) - s-users (list) - s-path (list) - s-rb (list) - file (list)
;Rec: outdated-system (list)
;Descripcion: Funcion auxiliar de add-file, añade archivos en caso de que no exista uno con el
;mismo nombre en la carpeta seleccionada en la ruta.
;Tipo de recursión: ninguna
(define add-file-aux (lambda (s-name s-drives s-users s-path s-rb) (lambda (file)
    (if (someone-logged-in? s-users)
        (set-system s-name (set-drives-rec (append file (list (get-logged-user s-users))) 
                                            sd-af s-drives s-path) 
                            s-users s-path s-rb)
        (set-system s-name s-drives s-users s-path s-rb)
    )
    ))
)

;Dom: outdated-system (list) - file (list)
;Rec: updated-system (list)
;Descripcion: Funcion que añade archivos a la carpeta actual donde se encuentra la ruta.
;Tipo de recursión: No empleada
(define add-file (lambda (outd-system) (lambda (file)
    ((add-file-aux (get-system-name outd-system) (get-system-drives outd-system) 
                   (get-system-users outd-system) (get-system-path outd-system)
                   (get-system-rb outd-system))
                    file)
    ))
)

;Dom: s-name (string) - s-drives (list) - s-users (list) - s-path (list) - s-rb (list) - filename (string)
;Rec: updated-system
;Descripcion: Funcion auxiiliar de del, elimina archivos o carpetas, ademas es capaz de eliminar
;archivos segun un patron, lo que elimine se ira a la papelera de reciclaje.
;Tipo de recursión: No empleada
(define del-aux (lambda (s-name s-drives s-users s-path s-rb) (lambda (filename)
    (if (someone-logged-in? s-users)
        (cond [(is-a-file-pattern? filename)
                (if (is-for-all-files? filename)
                    (set-system s-name (set-drives-rec filename sd-del-all-files s-drives s-path)
                                s-users s-path (move-all-files-to-rb filename s-drives s-path s-rb))
                    (set-system s-name (set-drives-rec filename sd-del-files-by-pattern s-drives s-path) 
                                s-users s-path (move-file-by-pattern-to-rb filename s-drives s-path s-rb))
                )]
            [(is-a-file? filename)
                (set-system s-name (set-drives-rec filename sd-del-file s-drives s-path) 
                            s-users s-path (move-file-to-rb filename s-drives s-path s-rb))]
            [else
                (set-system s-name (set-drives-rec filename sd-del-directory s-drives s-path) 
                            s-users s-path (move-directory-to-rb filename s-drives s-path s-rb))]
        )
        (set-system s-name s-drives s-users s-path s-rb)
    )
    ))
)

;Dom: outdated-system (list) - filename (string)
;Rec: updated-system
;Descripcion: Funcion que elimina carpetas o archivos y los manda a la papelera.
;Tipo de recursión: No empleada
(define del (lambda (outd-system) (lambda (filename)
    ((del-aux (get-system-name outd-system) (get-system-drives outd-system) 
              (get-system-users outd-system) (get-system-path outd-system)
              (get-system-rb outd-system))
               (string-downcase filename))
    ))
)

;Dom: s-name (string) - s-drives (list) - s-users (list) - s-path (list) - s-rb (list) - directory-name (string) - t-path (list)
;Rec: updated-system (list)
;Descripcion: Segunda funcion secundaria de rd, se encarga de eliminar la carpeta en caso
;en caso de que la ruta del sistema no este dentro de la ruta objetivo.
;Tipo de recursión: No empleada
(define rd-aux2 (lambda (s-name s-drives s-users s-path s-rb) (lambda (directory-name t-path)
    (if (and (someone-logged-in? s-users) (s-path-is-out-t-path? s-path t-path))
        (set-system s-name (set-drives-rec directory-name sd-rd s-drives t-path) s-users s-path s-rb)
        (set-system s-name s-drives s-users s-path s-rb)
    )
    ))
)

;Dom: s-name (string) - s-drives (list) - s-users (list) - s-path (list) - s-rb (list) - directory-name (string)
;Rec: updated-system (list)
;Descripcion: Primera funcion secundaria de rd, se encarga de extraer el nombre de la carpeta
;y su ruta en caso de ser necesario y en caso de que existan, luego se lo manda a la funcion secundaria
;Tipo de recursión: No empleada
(define rd-aux1 (lambda (s-name s-drives s-users s-path s-rb) (lambda (directory-name)
    (if (string-is-path? directory-name)
        (if (is-path-a-new-path? directory-name)
            (if (not (equal? s-path (set-path-new-path-rd directory-name s-path s-drives))) 
                ((rd-aux2 s-name s-drives s-users s-path s-rb)
                          (get-f-directory-from-sp-new-path directory-name s-path s-drives)
                          (set-path-new-path-rd directory-name s-path s-drives))
                (set-system s-name s-drives s-users s-path s-rb)    
            )
            (if (not (equal? s-path (set-path-add-path-rd directory-name s-path s-drives)))
                ((rd-aux2 s-name s-drives s-users s-path s-rb)
                          (get-f-directory-from-sp-add-path directory-name s-path s-drives)
                          (set-path-add-path-rd directory-name s-path s-drives))
                (set-system s-name s-drives s-users s-path s-rb)    
            )
        )
        ((rd-aux2 s-name s-drives s-users s-path s-rb) directory-name s-path)
    )
    ))
)

;Dom: outdated-system (list)
;Rec: updated-system (list)
;Descripcion: Funcion que elimina carpetas de forma permanente.
;Tipo de recursión: No empleada
(define rd (lambda (outd-system) (lambda (directory-name)
    ((rd-aux1 (get-system-name outd-system) (get-system-drives outd-system) 
             (get-system-users outd-system) (get-system-path outd-system)
             (get-system-rb outd-system))
             (string-downcase directory-name))
    ))
)

;-----------------------Otras operaciones-----------------------
;Dom: outdated-system (list) X function (function)
;Rec: updated-system (list)
;Descripcion: Funcion que permite ejecutar una funcion sobre el sistema.
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