#lang racket/base

(require math
         racket/contract
         (planet bzlib/date/plt)
         (file "/home/jkominek/forecasting/questions.rkt")
         (file "/home/jkominek/forecasting/rankings.rkt")
         (file "/home/jkominek/forecasting/opinions.rkt")
         (file "/home/jkominek/forecasting/utils.rkt")
         )

(question-database
 (load-question-database "/home/jkominek/forecasting/tiny.json"))

(define (cost->weight cost)
  (cond
    [(< cost 0) (+ 1 (abs cost))]
    [(< cost 1) 1]
    [else 1]))

