#lang racket
(require racket/date)

;-----------------------Representacion-----------------------
;Se presenta el TDA Fecha el cual corresponde tal y como indica su nombre a una representación
;de fecha en forma estructurada. Esta representacón esta dada por una serie de funciones las
;cuales tienen como objetivo obtener la fecha actual.

;-----------------------Representacion-----------------------
;Dom: null
;Rec: fecha-actual (string)
;Descripcion: Funcion que recopila todos los datos de la fecha actual y crea una fecha en forma de string
;Recursion: ninguno
(define (get-current-date)
    (string-append
        (get-current-day)
        "/"
        (get-current-month)
        "/"
        (get-current-year)
        " "
        (get-current-hour)
    )
)

;-----------------------Otras operaciones-----------------------
;Dom: null
;Rec: dia-actual (int)
;Descripcion: Funcion que obtiene el dia actual
;Recursion: ninguno
(define (get-current-day)
    (number->string (date-day (current-date)))
)

;Dom: null
;Rec: mes-actual (int)
;Descripcion: Funcion que obtiene el mes actual
;Recursion: ninguno
(define (get-current-month)
    (number->string (date-month (current-date)))
)

;Dom: null
;Rec: año-actual (int)
;Descripcion: Funcion que obtiene el año actual
;Recursion: ninguno
(define (get-current-year)
    (number->string (date-year (current-date)))
)

;Dom: null
;Rec: hora-actual (string)
;Descripcion: Funcion que obtiene la hora actual
;Recursion: ninguno
(define (get-current-hour)
    (string-append
        (number->string (date-hour (current-date)))
        ":"
        (number->string (date-minute (current-date)))
        ":"
        (number->string (date-second (current-date)))
    )
)

(provide get-current-date)