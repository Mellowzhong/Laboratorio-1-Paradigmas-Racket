#lang racket

;-----------------------Representacion-----------------------
;Se presenta el TDA User, el cual corresponde tal y como indica su nombre a una representacion
;del usuario, la cual contiene el nombre y el estado de "logeo". Esta representacion esta dada
;por una lista de esos elementos los cuales se modificaran dependiendo de lo que se le pida.

;-----------------------Pertenencia-----------------------
;Dom: username (string) - users-outdated-system (list)
;Rec: boolean
;Descripcion: Funcion busca si el nombre que se quiere seleccionar al nuevo usuario esta disponible.
;Tipo de recursión: No aplicada
(define (username-exists? username users-s)
    (not (boolean? (member username users-s)))
)

;Dom: users-outdated-system (list)
;Rec: boolean
;Descripcion: Funcion que busca si existe un usuario logeado.
;Tipo de recursión: No aplicada
(define (someone-logged-in? users-outdated-system)
    (not (boolean? (member #t users-outdated-system)))
)

;-----------------------Modificadores-----------------------
;Dom: username (string) - users-outdated-system (list)
;Rec: users-system (list)
;Descripcion: Funcion que actualiza la lista de los usuarios dentro del sistema cuando se quiere
;agregar un nuevo usuario.
;Tipo de recursión: No aplicada
(define (set-users-system-reg username users-s)
    (append users-s (list username #f))
)

;Dom: username (string) - users-outdated-system (list)
;Rec: users-system (list)
;Descripcion: Funcion que actualiza la lista modificando el estado de logeado del usuario.
;Tipo de recursión: No aplicada
(define (set-users-system-login username users-outdated-system)
    (append
        (reverse (member username (reverse users-outdated-system)))
        (list #t)
        (remove #f (remove username (member username users-outdated-system))))
)

;Dom: users-outdated-system (list)
;Rec: users-system (list)
;Descripcion: Funcion que actualiza la lista deslogeando al usuario logeado.
;Tipo de recursión: No aplicada
(define (set-users-system-logout users-outdated-system)
    (append
        (remove #t (reverse (member #t (reverse users-outdated-system))))
        (list #f)
        (remove #t (member #t users-outdated-system)))
)

;-----------------------Otras operaciones-----------------------
;Dom: s-users (list)
;Rec: username (list)
;Descripcion: Funcion que retorna el username del usuario logueado.
;Tipo de recursión: No aplicada
(define (get-logged-user s-users)
    (list-ref (member #t (reverse s-users)) 1)
)

(provide (all-defined-out))