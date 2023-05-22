#lang racket

(require "TDA_Drives_212515965_Sanchez.rkt")

;-----------------------Representacion-----------------------
;Se presenta el TDA Recycle bin, el cual corresponde tal y como indica su nombre a una representacion
;de la papelera de reciclaje, el cual contiene los archivos borrados del sistema. Esta representacion
;esta dada por una serie de funciones las cuales tienen como objetivo mover archivos hacia la papelera de reciclaje.

;-----------------------Modificadores-----------------------

;Dom: filename (string) - system-drives (list) - system-rb (list)
;Rec: updated-system-rb
;Descripcion: Funcion que agrega un archivo a la papelera de reclaje
;Tipo de recursión: No empleada
(define (move-file-to-rb filename system-drives system-path system-rb)
    (move-file-to-rb-aux filename (get-last-directory-from-path system-path system-drives)
                                  system-path system-rb)
)

;Dom: filename (string) - directory (list) - path-system (list) - recycle-bin-system (list)
;Rec: updated-recycle-bin-system (list)
;Descripcion: Funcion que mueve todos los archivos de una carpeta a la papelera de reciclaje.
;Tipo de recursión: No empleada
(define (move-all-files-to-rb filename system-drives system-path recycle-bin-system)
    (move-all-files-to-rb-aux (get-last-directory-from-path system-path system-drives) 
                               system-path recycle-bin-system)
)

;Dom: file-pattern (string) X directory (list) X path-system (list) X recycle-bin-system (list)
;Rec: updated-recycle-bin-system (list)
;Descripcion: Funcion que mueve todos los archivos de una carpeta que cumplen un patron a la
;papelera de reciclaje.
;Tipo de recursión: No empleada
(define (move-file-by-pattern-to-rb file-pattern system-drives system-path system-rb)
    (move-file-by-pattern-to-rb-aux (get-file-pattern-info file-pattern)
                                    (get-last-directory-from-path system-path system-drives)
                                    system-path system-rb)
)

;-----------------------Otras operaciones-----------------------
;Dom: filename (string) - directory (list) - path-system (list) - recycle-bin-system (list)
;Rec: updated-recycle-bin-system (list)
;Descripcion: Funcion auxiliar de move-file-to-rb, mueve un archivo a la papelera de reciclaje.
;Tipo de recursión: No empleada
(define (move-file-to-rb-aux filename directory path-system recycle-bin-system)
    (if (source-exists? filename directory)
        (if (not (hidden-file? filename directory))
            (append recycle-bin-system (list (list (list-ref (member filename directory) 0)
                                            (list-ref (member filename directory) 1)
                                            (list-ref (member filename directory) 2)
                                            (list-ref (member filename directory) 3))
                                       path-system))
            recycle-bin-system
        )
        recycle-bin-system
    )
)

;Dom: directory (list) - path-system (list) - recycle-bin-system (list)
;Rec: updated-recycle-bin-system (list)
;Descripcion: Funcion auxiliar de move-all-files-to-rb,
;mueve todos los archivos de una carpeta a la papelera de reciclaje.
;Tipo de recursión: cola
(define (move-all-files-to-rb-aux directory path-system recycle-bin-system)
    (if (null? directory)
        recycle-bin-system
        (if (string? (car directory))
            (if (is-a-file? (car directory))
                (move-all-files-to-rb-aux (cdr (cddddr directory)) path-system
                                          (append recycle-bin-system 
                                                  (list (list (list-ref directory 0) 
                                                              (list-ref directory 1)
                                                              (list-ref directory 2)
                                                              (list-ref directory 3))
                                                        path-system)))
                (move-all-files-to-rb-aux (cdr directory) path-system recycle-bin-system)
            )
        (move-all-files-to-rb-aux (cdr directory) path-system recycle-bin-system)
        )
    )
)

;Dom: pattern-info (list) X directory (list) X path-system (list) X recycle-bin-system (list)
;Rec: updated-recycle-bin-system (list)
;Descripcion: Funcion aux de move-file-by-pattern-to-rb, mueve todos los archivos
;de una carpeta que cumplen un patron a la papelera de reciclaje.
;Tipo de recursión: cola
(define (move-file-by-pattern-to-rb-aux pattern-info directory path-system recycle-bin-system)
    (if (null? directory)
        recycle-bin-system
        (if (string? (car directory))
            (if (and (is-a-file? (car directory)) 
                     (string=? (list-ref directory 1) (list-ref pattern-info 1))
                     (file-meets-string-pattern? (car directory) pattern-info))
                (move-file-by-pattern-to-rb-aux pattern-info (cdr (cddddr directory))
                                                path-system
                                                (append recycle-bin-system 
                                                        (list (list (list-ref directory 0)
                                                                    (list-ref directory 1)
                                                                    (list-ref directory 2)
                                                                    (list-ref directory 3))
                                                               path-system)))
                (move-file-by-pattern-to-rb-aux pattern-info (cdr directory) path-system
                                                recycle-bin-system)
            )
            (move-file-by-pattern-to-rb-aux pattern-info (cdr directory) path-system
                                            recycle-bin-system)
        )
    )
)

;Dom: directory-name (string) - act-directory (list) - path-system (list) - recycle-bin-system (list)
;Rec: updated-recycle-bin-system (list)
;Descripcion: Funcion aux. de move-directory-to-rb,
;mueve una carpeta indicada en la carpeta seleccionada.
;Tipo de recursión: cola
(define (move-directory-to-rb-aux directory-name actual-directory path-system recycle-bin-system)
    (if (null? actual-directory)
        recycle-bin-system
        (if (string? (car actual-directory))
            (if (string=? (car actual-directory) directory-name)
                (move-directory-to-rb-aux directory-name (cddr actual-directory) path-system 
                                          (append recycle-bin-system
                                                  (list (list (list-ref actual-directory 0)
                                                              (list-ref actual-directory 1)
                                                              (list-ref actual-directory 2))
                                                        path-system)))
                (move-directory-to-rb-aux directory-name (cdr actual-directory) path-system
                                          recycle-bin-system)
            )
        (move-directory-to-rb-aux directory-name (cdr actual-directory) path-system
                                  recycle-bin-system)
        )
    )
)

;Dom: directory-name (string) - act-directory (list) - path-system (list) - recycle-bin-system (list)
;Rec: updated-recycle-bin-system (list)
;Descripcion: Funcion que mueve una carpeta indicada en la carpeta seleccionada.
;Tipo de recursión: No empleada
(define (move-directory-to-rb directory-name system-drives system-path system-rb)
    (move-directory-to-rb-aux directory-name (get-last-directory-from-path system-path system-drives)
                              system-path system-rb)
)

(provide (all-defined-out))