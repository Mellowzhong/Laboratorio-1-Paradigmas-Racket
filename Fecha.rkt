#lang racket
(provide (all-defined-out))
(require racket/date)

(define date-now (current-date))

(define list-date
    (list
      (date-year date-now)
      (date-month date-now)
      (date-day date-now)
      )
  )
  
(define fecha (string-append (number->string(list-ref list-date 0)) "/"
                            (number->string(list-ref list-date 1)) "/"
                            (number->string(list-ref list-date 2))
                )
  )

