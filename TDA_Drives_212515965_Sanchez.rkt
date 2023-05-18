#lang racket
;-----------------------Representacion-----------------------
;Se presenta el TDA Drives, el cual corresponde a tal y como indica su nombre a una representacion del drive,
;el drive es el lugar donde se almacenan las unidades, las carpetas o archivos varios. Esta representacion esta dada
;por una serie de funciones las cuales tendrian el fin de encontrar o modificar cosas de los drives.

;-----------------------Constructor-----------------------
;Dom: directory-info (list) - directory (list)
;Rec: updated-directory (list)
;Descripcion: Funcion que crea una carpeta en la carpeta seleccionada
;Tipo de recursión: No empleada
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

;-----------------------Pertenencia-----------------------
;Dom: filename (string)
;Rec: boolean
;Descripcion: Funcion que identifica si el patron selecciona a todos los archivos de la carpeta.
;Tipo de recursión: No empleada
(define (is-for-all-files? filename)
    (if (string=? filename "*.*")
        #t
        #f
    )
)

;Dom: filename (string)
;Rec: boolean
;Descripcion: Funcion que identifica si el nombre del archivo es un patron, primero revisa
;el caso en que sea para todos los archivos.
;Tipo de recursión: ninguna
(define (is-a-file-pattern? filename)
    (if (string=? filename "*.*")
        #t
        (is-a-file-pattern?-aux (string->list filename)))
)

;Dom: filename (string)
;Rec: boolean
;Descripcion: Funcion que identifica si filename es un archivo
;Tipo de recursión: No empleada
(define (is-a-file? filename)
    (is-a-file?-aux (string->list filename))
)

;Dom: source-name (char or string) - current-directory (list)
;Rec: boolean
;Descripcion: Funcion que identifica si existe una unidad, archivo o carpeta en la carpeta indicada.
;Tipo de recursión: No empleada
(define (source-exists? source-name current-directory)
    (not (boolean? (member source-name current-directory)))
)

;Dom: filename (string) - directory (list)
;Rec: boolean
;Descripcion: Funcion que identifica si un archivo esta oculto.
;Tipo de recursión: No empleada
(define (hidden-file? filename directory)
    (if (list-ref (list-ref (member filename directory) 3) 1)
        #t #f
    )
)

;Dom: filename (string) - directory (list)
;Rec: boolean
;Descripcion: Funcion que identifica si un archivo solo se puede leer.
;Tipo de recursión: No empleada
(define (only-read-file? filename directory)
    (if (list-ref (list-ref (member filename directory) 3) 2)
        #t #f
    )
)

;Dom: directory-name (string) - directory (list)
;Rec: boolean
;Descripcion: Funcion que identifica si una carpeta esta oculta.
;Tipo de recursión: No empleada
(define (hidden-directory? directory-name directory)
    (if (list-ref (list-ref (member directory-name directory) 2) 3)
        #t #f
    )
)

;Dom: directory-name (string) - directory (list)
;Rec: boolean
;Descripcion: Funcion que identifica si una carpeta solo se puede leer.
;Tipo de recursión: No empleada
(define (only-read-directory? directory-name directory)
    (if (list-ref (list-ref (member directory-name directory) 2) 4)
        #t #f
    )
)

;Dom: file-name (string) - patter-info (list)
;Rec: boolean
;Descripcion: Funcion que identifica si un archivo cumple el patrion de string.
;Tipo de recursión: No empleada
(define (file-meets-string-pattern? filename pattern-info)
    (if (null? (list-ref pattern-info 0))
            #t
        (if (regexp-match
            (string-append "^" (list-ref pattern-info 0))
            filename
            )
            #t
            #f
        )
    )
)

;Dom: directory-name (string) - directory (list)
;Rec: boolean
;Descripcion: Funcion que revisa si una carpeta esta vacia
;Tipo de recursión: No empleada
(define (empty-directory? directory-name directory)
    (if (null? (list-ref (member directory-name directory) 1))
        #t
        #f
    )
)

;-----------------------Selectores-----------------------
;Dom: filename (string) - actual-directory (list)
;Rec: file (list)
;Descripcion: Funcion que consigue el archivo indicado en la carpeta.
;Tipo de recursión: No empleada
(define (get-file filename actual-directory)
    (get-file-aux filename actual-directory null)
)

;Dom: directory-name (string) - actual-directory (list)
;Rec: directory (list)
;Descripcion: Funcion que obtiene la carpeta indicada en la carpeta seleccionada.
;Tipo de recursión: No empleada
(define (get-directory directory-name current-directory)
    (get-directory-aux directory-name current-directory null)
)
;Dom: d-letter (char) - s-drives (list)
;Rec: t-drive-info
;Descripcion: Funcion que obtiene el nombre y contenido de la unidad seleccionada.
;Tipo de recursión: No empleada
(define (get-t-drive-from-s-path d-letter s-drives)
    (list (list-ref (member d-letter s-drives) 1)
          (list-ref (member d-letter s-drives) 3))
)

;Dom: s-path (list) - s-drives (list)
;Rec: t-directory-info (list)
;Descripcion: Funcion que obtiene el nombre y contenido de la carpeta seleccionada.
;Tipo de recursión: No empleada
(define (get-t-directory-from-s-path s-path s-drives)
    (get-t-directory-from-s-path-aux (last s-path) 
                                     (get-last-directory-from-path (cdr (reverse s-path)) s-drives))
)

;Dom: letter-selected-drive (char) - drives-system (list)
;Rec: selected-directory (list)
;Descripcion: Funcion que obtiene los archivos y carpetas de la unidad selecciada en la ruta.
;Rec: No empleada
(define (get-selected-directory-drive letter-selected-drive drives-system)
    (list-ref (member letter-selected-drive drives-system) 3)
)

;Dom: name-selected-directory (string) - current-directory (list)
;Rec: selected-directory (list)
;Descripcion: Funcion que obtiene los archivos y carpetas de la carpeta seleccionada en la ruta.
;Tipo de recursión: No empleada
(define (get-selected-directory-directory name-selected-directory current-directory)
    (list-ref (member name-selected-directory current-directory) 1)
)

;Dom: current-path (list) - current-directory (list)
;Rec: selected-directory (list)
;Descripcion: Funcion que obtiene los archivos y carpetas de la ultima carpeta seleccionada en la ruta dada.
;Tipo de recursión: cola
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

;Dom: filepattern (string)
;Rec: filepattern-info (list)
;Descripcion: Funcion que retorna una lista con toda la informacion del patron.
;Tipo de recursión: No empleada
(define (get-file-pattern-info filepattern)
    (get-file-pattern-info-aux (string->list filepattern) null null)
)

;-----------------------Modificadores-----------------------
;Dom: drive-letter (char) - drive-name (string) - drive-capacity (int) - system-drives (list)
;Rec: system-drives (list)
;Descripcion: Funcion que actualiza la lista de unidades cuando se añade una nueva unidad.
;Tipo de recursión: No empleada
(define (set-drives-add-drive letter-d name-d capacity-d drives-s)
    (append drives-s (list letter-d name-d capacity-d (list)))
)

;Dom: source (string or list) - fn (function) - current-directory (list) - current-path (list)
;Rec: system-drives (list)
;Descripcion: Funcion que actualiza la lista de unidades recorriendola segun la ruta indicada
;cuando se llega al final de la ruta se aplica una funcion.
;Tipo de recursión: natural
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

;Dom: source (list) - source-name (string) - t-path (list) - s-drives (list) - s-path (list)
;Rec: system-drives
;Descripcion: Funcion que actualiza la lista de unidades 2 veces consecutivas
;la primera usando la funcion de eliminar un archivo o carpeta, esto segun
;la ruta del sistema, y luego la funcion de copiar un archivo, esto en otra ruta.
;Tipo de recursión: No empleada
(define (set-drives-move source source-name t-path s-drives s-path)
    (if (is-a-file? source-name)
        (set-drives-rec source sd-copy (set-drives-rec source-name sd-del-file s-drives s-path) t-path)
        (set-drives-rec source sd-copy (set-drives-rec source-name sd-del-directory s-drives s-path) t-path)
    )
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

;Dom: directory (list)
;Rec: updated-directory (list)
;Descripcion: Funcion que borra todos los archivos dentro de una carpeta
;Tipo de recursión: No empleada
(define (sd-del-all-files filename directory)
    (sd-del-all-files-aux directory null)
)

;Dom: filepattern (string) - directory (list)
;Rec: updated-directory
;Descripcion: Funcion que elimina todos los archivos segun un patron dentro de una carpeta.
;Tipo de recursión: No empleada
(define (sd-del-files-by-pattern filepattern directory)
    (sd-del-files-by-pattern-aux (get-file-pattern-info filepattern) directory null)
)

;Dom: filename (string) - directory (list)
;Rec: directory (list)
;Descripcion: Funcion que elimina un archivo en concreto
;Tipo de recursión: No empleada
(define (sd-del-file filename directory)
    (if (and (source-exists? filename directory) (not (hidden-file? filename directory)))
        (sd-del-file-aux filename directory null)
        directory
    )
)

;Dom: directory-name (string) - directory (list)
;Rec: directory (list)
;Descripcion: Funcion que elimina una carpeta con la funcion del.
;Tipo de recursión: No empleada
(define (sd-del-directory directory-name directory)
        (if (source-exists? directory-name directory)
            (sd-del-directory-aux directory-name directory null)
            directory
        )
)

;Dom: directory-name (string) - directory (list)
;Rec: directory (list)
;Descripcion: Funcion que elimina una carpeta solamente si esta vacia
;Tipo de recursión: No empleada
(define (sd-rd directory-name directory)
    (if (and (source-exists? directory-name directory)
             (empty-directory? directory-name directory))
        (sd-rd-aux directory-name directory null)
        directory
    )
)

;Dom: source (list) - directory (list)
;Rec: updated-directory (list)
;Descripcion: Funcion que copia un archivo o carpeta proveniente de otra parte de la lista de unidades.
;Tipo de recursión: No empleada
(define (sd-copy source directory)
    (append directory source)
)

;Dom: source-info (list) - directory (list)
;Rec: updated-directory (list)
;Descripcion: funcion que cambia el nombre de un archivo o carpeta si el nuevo nombre no existe.
;Tipo de recursión: No empleada
(define (sd-ren source-info directory)
    (if (not (source-exists? (list-ref source-info 1) directory))
        (sd-ren-aux (list-ref source-info 0) (list-ref source-info 1) directory null)
        directory
    )
)

;Dom: d-letter (char) - new-d-name (string) - system-drives (list)
;Rec: system-drives (list)
;Descripcion: Funcion que actualiza la lista de unidades modificando la unidad con la letra seleccionada.
;Tipo de recursión: No empleada
(define (set-drives-format d-letter new-d-name system-drives)
    (set-drives-format-aux d-letter new-d-name system-drives null)
)

;Dom: filename-list (list) - l-char (list) - name (string)
;Rec: renamed-file (string)
;Descripcion: Funcion que modifica el nombre de una archivo añadiendole un 1 al final.
;Tipo de recursión: cola
(define (rename-copy-file filename-list l-char name)
    (if (null? filename-list)
        (string-append name "." (list->string l-char))
        (if (equal? (car filename-list) #\.)
            (rename-copy-file (cdr filename-list) null (list->string (append l-char (list #\1))))
            (rename-copy-file (cdr filename-list) (append l-char (list (car filename-list))) name)
        )
    )
)

;Dom: file (list)
;Rec: file-but-renamed (list)
;Descripcion: Funcion que renombra un archivo si ya existe uno con el mismo nombre.
;Tipo de recursión: No empleada
(define (get-and-ren-file filename actual-directory)
    (append (list (rename-copy-file (string->list (car (get-file filename actual-directory)))
                                        null null)) 
            (cdr (get-file filename actual-directory)))
)

;Dom: directory (list)
;Rec: directory-but-renamed
;Descripcion: Funcion que renombra una carpeta si ya existe una con el mismo nombre.
;Tipo de recursión: No empleada
(define (get-and-ren-directory directory-name actual-directory)
    (append (list (string-append (car (get-directory directory-name actual-directory)) "1")) 
                  (cdr (get-directory directory-name actual-directory)))
)

;-----------------------Otras funciones-----------------------
;Dom: filepattern-l (list) - l-2 (list) - l-3 (list)
;Rec: file-pattern-info (list)
;Descripcion: Funcion auxiliar de get-file-pattern-info,
;recoge la informacion que contiene el patron de archivos.
;Tipo de recursión: cola
(define (get-file-pattern-info-aux filepattern-l l-2 l-3)
    (if (null? filepattern-l)
        (append l-3 (list (list->string l-2)))
        (if (equal? (car filepattern-l) #\*)
            (if (null? l-2)
                (get-file-pattern-info-aux (cddr filepattern-l) null (append l-3 (list null)))
                (get-file-pattern-info-aux (cddr filepattern-l) null
                                           (append l-3 (list (list->string l-2))))
            )
            (get-file-pattern-info-aux (cdr filepattern-l) (append l-2 (list (car filepattern-l)))
                                        l-3)
        )
    )
)

;Dom: filename-l (list)
;Rec: boolean
;Descripcion: Funcion auxiliar de is-a-file-pattern?, identifica si el filename es un patron.
;Tipo de recursión: cola
(define (is-a-file-pattern?-aux filename-l)
    (if (null? filename-l)
        #f
        (if (equal? (car filename-l) #\*)
            #t
            (is-a-file-pattern?-aux (cdr filename-l))
        )
    )
)

;Dom: filename-l (list)
;Rec: boolean
;Descripcion: Funcion auxiliar de is-a-file?, busca comprobar si el filename es de un archivo
;Tipo de recursión: cola
(define (is-a-file?-aux filename-l)
    (if (null? filename-l)
        #f
        (if (equal? (car filename-l) #\.)
            #t
            (is-a-file?-aux (cdr filename-l))
        )
    )
)

;Dom: filename (string) - actual-directory (list) - file (null)
;Rec: file (list)
;Descripcion: Funcion auxiliar de get-file, busca el archivo indicada en la ruta seleccionada.
;Tipo de recursión: cola
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

;Dom: directory-name (string) - actual-directory (list) - directory (null)
;Rec: directory (list)
;Descripcion: Funcion auxiliar de get-directory busca la carpeta indicada en la ruta seleccionada.
;Tipo de recursión: cola
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

;Dom: t-directory-name (string) - current-directory (list)
;Rec: t-directory-info (list)
;Descripcion: Funcion auxiliar de get-t-directory-from-s-path, guarda el nombre y el contenido de la carpeta.
;Tipo de recursión: No empleada
(define (get-t-directory-from-s-path-aux t-directory-name current-directory)
    (list (list-ref (member t-directory-name current-directory) 0)
          (list-ref (member t-directory-name current-directory) 1))
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
;Dom: source-name (string) - n-source-name (string) - directory (list) - l-aux (list)
;Rec: updated-directory (list)
;Descripcion: Funcion auxiliar de sd-ren, cambia el nombre de un archivo o carpeta, en caso
;de que el nuevo nombre no exista.
;Tipo de recursión: cola
(define (sd-ren-aux source-name n-source-name directory l-aux)
    (if (null? directory)
        l-aux
        (if (and (string? (car directory)) (string=? source-name (car directory)))
            (sd-ren-aux source-name n-source-name (cdr directory) 
            (append l-aux (list n-source-name)))
            (sd-ren-aux source-name n-source-name (cdr directory) 
            (append l-aux (list (car directory))))
        )
    )
)
;Dom: directory-name (string) - directory (list) - l-2 (list)
;Rec: directory (list)
;Descripcion: Funcion auxiliar de sd-rd, elimina una carpeta en el caso de que este vacia.
;Tipo de recursión: cola
(define (sd-rd-aux directory-name directory l-2)
    (if (null? directory)
        l-2
        (if (string? (car directory))
            (if (string=? (car directory) directory-name)
                (sd-rd-aux directory-name (cdddr directory) l-2)
                (sd-rd-aux directory-name (cdr directory)
                                      (append l-2 (list (car directory))))
            )
            (sd-rd-aux directory-name (cdr directory) (append l-2 (list (car directory))))
        )
    )
)
;Dom: directory-name (string) - directory (list) - l-aux (list)
;Rec: directory (list)
;Descripcion: Funcion auxiliar de sd-del-directory, elimina la carpeta que 
;esta dentro de la carpeta seleccionada.
;Tipo de recursión: cola
(define (sd-del-directory-aux directory-name directory l-aux)
    (if (null? directory)
        l-aux
        (if (and (string? (car directory)) (string=? directory-name (car directory)))
            (sd-del-directory-aux directory-name (cdddr directory) l-aux)
            (sd-del-directory-aux directory-name (cdr directory) (append l-aux (list (car directory))))
        )
    )
)
;Dom: filename (string) - directory (list) - l-aux (list)
;Rec: updated-directory
;Descripcion: Funcion auxiliar de sd-del-file, elimina el archivo indicado dentro de la carpeta.
;Tipo de recursión: cola
(define (sd-del-file-aux filename directory l-aux)
    (if (null? directory)
        l-aux
        (if (and (string? (car directory)) (string=? filename (car directory)))
            (sd-del-file-aux filename (cddddr directory) l-aux)
            (sd-del-file-aux filename (cdr directory) (append l-aux (list (car directory))))
        )
    )
)

;Dom: directory (list) - l-aux (list)
;Rec: directory (list)
;Descripcion: Funcion auxiliar de sd-all-files, elimina todos los archivos que encuentre dentro de la carpeta.
;Tipo de recursión: cola
(define (sd-del-all-files-aux directory l-aux)
    (if (null? directory)
        l-aux
        (if (string? (car directory))
            (if (is-a-file? (car directory)) 
                (sd-del-all-files-aux (cddddr directory) l-aux)
                (sd-del-all-files-aux (cdr directory) (append l-aux (list (car directory))))
            )
            (sd-del-all-files-aux (cdr directory) (append l-aux (list (car directory))))
        )
    )
)

;Dom: pattern-info (list) - directory (list) - l-aux (list)
;Rec: directory (list)
;Descripcion: Funcion auxiliar de sd-del-files-by-pattern, elimina archivos segun un patron.
;Tipo de recursión: cola
(define (sd-del-files-by-pattern-aux pattern-info directory l-2)
    (if (null? directory)
        l-2
        (if (string? (car directory))
            (if (and (is-a-file? (car directory))
                     (string=? (list-ref directory 1) (list-ref pattern-info 1))
                     (file-meets-string-pattern? (car directory) pattern-info)
                     (not (hidden-file? (car directory) directory)))
                (sd-del-files-by-pattern-aux pattern-info (cddddr directory) l-2)
                (sd-del-files-by-pattern-aux pattern-info (cdr directory)
                                                       (append l-2 (list (car directory))))
            )
            (sd-del-files-by-pattern-aux pattern-info (cdr directory)
                                                   (append l-2 (list (car directory))))
        )
    )
)

(provide (all-defined-out))