#lang racket
(require racket/date)

;Dominio: racket/date
;Recorrido: date (list)
;Descripción: Ocupa el requerimiento de racket/date para determinar la fecha actual
;Luego saca el año, mes y dia para ponerlos en una lista
(define list-date
    (list
      (date-year (current-date))
      (date-month (current-date))
      (date-day (current-date))
      )
  )

;Dominio: list-date
;Recorrido: date (string)
;Descripción: Agarra la lista antes obtenida y la convierte en un string
(define date-now (string-append (number->string(list-ref list-date 0)) "/"
                            (number->string(list-ref list-date 1)) "/"
                            (number->string(list-ref list-date 2))
                )
  )

(provide (all-defined-out))

