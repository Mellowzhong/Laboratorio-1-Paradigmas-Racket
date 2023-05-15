#lang racket
;-----------------------Representacion-----------------------
;Se presenta el TDA Drives, el cual corresponde a tal y como indica su nombre a una representacion del drive,
;el drive es el lugar donde se almacenan las unidades, las carpetas o archivos varios. Esta representacion esta dada
;por una serie de funciones las cuales tendrian el fin de encontrar o modificar cosas de los drives.

;-----------------------Constructor-----------------------
;Dom: directory-info (list) - directory (list)
;Rec: updated-directory (list)
;Descripcion: Funcion que crea una carpeta en la carpeta seleccionada
;Recursion: ninguno
(define (sd-md directory-info directory)
    (if (not (source-exists? (list-ref directory-info 0) directory))
        (append directory (list (list-ref directory-info 0) null
                                      (list (list-ref directory-info 1)
                                            (list-ref directory-info 1)
                                            (list-ref directory-info 2)
                                            (list-ref directory-info 3)
                                            (list-ref directory-info 4))))
        directory
    )
)

;-----------------------Modificadores-----------------------
;Dom: drive-letter (char) - drive-name (string) - drive-capacity (int) - system-drives (list)
;Rec: system-drives (list)
;Descripcion: Funcion que actualiza la lista de unidades cuando se añade una nueva unidad.
;Recursion: ninguno
(define (set-drives-add-drive letter-d name-d capacity-d drives-s)
    (append drives-s (list letter-d name-d capacity-d (list)))
)

;Dom: source (string or list) - fn (function) - current-directory (list) - current-path (list)
;Rec: system-drives (list)
;Descripcion: Funcion que actualiza la lista de unidades recorriendola segun la ruta indicada
;cuando se llega al final de la ruta se aplica una funcion.
;Recursion: natural
(define (set-drives-rec directory-name fn current-directory current-path)
    (if (null? current-path)
        (fn directory-name current-directory)
        (if (char? (car current-path))
            (append (reverse (member (list-ref (member (car current-path) current-directory) 2) 
                             (reverse current-directory)))
                    (list (set-drives-rec directory-name fn
                                         (list-ref (member (car current-path) current-directory) 3)
                                         (cdr current-path)))
                    (cdr (member (list-ref (member (car current-path) current-directory) 3) 
                          current-directory)))
            (append (reverse (member (car current-path) (reverse current-directory)))
                    (list (set-drives-rec directory-name fn
                                         (list-ref (member (car current-path) current-directory) 1)
                                         (cdr current-path)))
                    (cddr (member (car current-path) current-directory)))
        )
    )
)

;Dom: d-letter (char) - new-d-name (string) - system-drives (list) - l-aux (list)
;Rec: system-drives
;Descripcion: Funcion auxiliar de set-drives-format, formatea la unidad con la letra indicada
;cambiandole el nombre, borrando todos los archivos que contiene y manteniendo su almacenamiento.
;Tipo de recursión: cola
(define (set-drives-format-aux d-letter new-d-name system-drives l-aux)
    (if (null? system-drives)
        l-aux
        (if (and (char? (car system-drives)) (equal? d-letter (car system-drives)))
            (set-drives-format-aux d-letter new-d-name (cddddr system-drives)
                                   (append l-aux (list d-letter new-d-name
                                                       (list-ref system-drives 2) null)))
            (set-drives-format-aux d-letter new-d-name (cdr system-drives)
                                   (append l-aux (list (car system-drives))))
        )
    )
)

;Dom: d-letter (char) - new-d-name (string) - system-drives (list)
;Rec: system-drives (list)
;Descripcion: Funcion que actualiza la lista de unidades modificando la unidad con la letra seleccionada.
;Tipo de recursión: No empleada
(define (set-drives-format d-letter new-d-name system-drives)
    (set-drives-format-aux d-letter new-d-name system-drives null)
)

;Dom: file (list) - directory (list)
;Rec: updated-directory (list)
;Descripcion: Funcion que añade un archivo en la carpeta seleccionada
;Tipo de recursión: No empleada
(define (sd-af file directory)
    (if (not (source-exists? (car file) directory))
        (append directory (list (list-ref file 0) (list-ref file 1) (list-ref file 2)
                                (list (list-ref file 5) (list-ref file 3) (list-ref file 4))))
        directory
    )
)
;-----------------------Pertenencia-----------------------
;Dom: source-name (char or string) - current-directory (list)
;Rec: boolean
;Descripcion: Funcion que identifica si existe una unidad, archivo o carpeta en la carpeta indicada.
;Recursion: ninguno
(define (source-exists? source-name current-directory)
    (not (boolean? (member source-name current-directory)))
)

;Dom: directory-name (string) - directory (list)
;Rec: boolean
;Descripcion: Funcion que revisa si una carpeta esta vacia
;Recursion: ninguno
(define (empty-directory? directory-name directory)
    (if (null? (list-ref (member directory-name directory) 1))
        #t
        #f
    )
)
;-----------------------Otras operaciones-----------------------
;Dom: letter-selected-drive (char) - drives-system (list)
;Rec: selected-directory (list)
;Descripcion: Funcion que obtiene los archivos y carpetas de la unidad selecciada en la ruta.
;Rec: ninguno
(define (get-selected-directory-drive letter-selected-drive drives-system)
    (list-ref (member letter-selected-drive drives-system) 3)
)

;Dom: name-selected-directory (string) - current-directory (list)
;Rec: selected-directory (list)
;Descripcion: Funcion que obtiene los archivos y carpetas de la carpeta seleccionada en la ruta.
;Recursion: ninguno
(define (get-selected-directory-directory name-selected-directory current-directory)
    (list-ref (member name-selected-directory current-directory) 1)
)

;Dom: current-path (list) - current-directory (list)
;Rec: selected-directory (list)
;Descripcion: Funcion que obtiene los archivos y carpetas de la ultima carpeta seleccionada en la ruta dada.
;Recursion: cola
(define (get-last-directory-from-path current-path current-directory)
    (if (null? current-path)
        current-directory
        (if (char? (list-ref current-path 0))
            (get-last-directory-from-path (cdr current-path)
                (get-selected-directory-drive (list-ref current-path 0) current-directory))
            (get-last-directory-from-path (cdr current-path)
                (get-selected-directory-directory (list-ref current-path 0) current-directory))
        )
    )
)

;Dom: filename-l (list)
;Rec: boolean
;Descripcion: Funcion auxiliar de is-a-file?, busca comprobar si el filename es de un archivo
;Recursion: cola
(define (is-a-file?-aux filename-l)
    (if (null? filename-l)
        #f
        (if (equal? (car filename-l) #\.)
            #t
            (is-a-file?-aux (cdr filename-l))
        )
    )
)

;Dom: filename (string)
;Rec: boolean
;Descripcion: Funcion que identifica si filename es un archivo
;Recursion: ninguno
(define (is-a-file? filename)
    (is-a-file?-aux (string->list filename))
)

;Dom: filename (string) - actual-directory (list) - file (null)
;Rec: file (list)
;Descripcion: Funcion auxiliar de get-file, busca el archivo indicada en la ruta seleccionada.
;Recursion: cola
(define (get-file-aux filename actual-directory file)
    (if (null? actual-directory)
        file
        (if (string? (car actual-directory))
            (if (string=? (car actual-directory) filename)
                (get-file-aux filename (cdr actual-directory) 
                              (append file (list (list-ref actual-directory 0)
                                                 (list-ref actual-directory 1)
                                                 (list-ref actual-directory 2)
                                                 (list-ref actual-directory 3))))
                (get-file-aux filename (cdr actual-directory) file)
            )
            (get-file-aux filename (cdr actual-directory) file)
        )
    )
)

;Dom: filename (string) - actual-directory (list)
;Rec: file (list)
;Descripcion: Funcion que consigue el archivo indicado en la carpeta.
;Recursion: ninguno
(define (get-file filename actual-directory)
    (get-file-aux filename actual-directory null)
)

;Dom: directory-name (string) - actual-directory (list) - directory (null)
;Rec: directory (list)
;Descripcion: Funcion auxiliar de get-directory busca la carpeta indicada en la ruta seleccionada.
;Recursion: cola
(define (get-directory-aux directory-name current-directory directory)
    (if (null? current-directory)
        directory
        (if (string? (car current-directory))
            (if (string=? (car current-directory) directory-name)
                (get-directory-aux directory-name (cdr current-directory) 
                              (append directory (list (list-ref current-directory 0)
                                                      (list-ref current-directory 1)
                                                      (list-ref current-directory 2))))
                (get-directory-aux directory-name (cdr current-directory) directory)
            )
            (get-directory-aux directory-name (cdr current-directory) directory)
        )
    )
)

;Dom: directory-name (string) - actual-directory (list)
;Rec: directory (list)
;Descripcion: Funcion que obtiene la carpeta indicada en la carpeta seleccionada.
;Recursion: ninguno
(define (get-directory directory-name current-directory)
    (get-directory-aux directory-name current-directory null)
)


(provide (all-defined-out))