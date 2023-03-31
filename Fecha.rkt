#lang racket
(provide (all-defined-out))
(require racket/date)

;Identifica la fecha actual
(define date-now (current-date))

;Lleva la fecha actual a una lista
(define list-date
    (list
      (date-year date-now)
      (date-month date-now)
      (date-day date-now)
      )
  )

;Transforma la lista de la fecha a un string
(define fecha (string-append (number->string(list-ref list-date 0)) "/"
                            (number->string(list-ref list-date 1)) "/"
                            (number->string(list-ref list-date 2))
                )
  )

