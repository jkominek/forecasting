#lang racket/base

(require math
	 (planet bzlib/date/plt)
	 racket/contract)

(define/contract
  (normalize-probabilities l)
  (-> (listof (>=/c 0)) (listof (real-in 0 1)))

  (let ([sum (apply + l)])
    (map (lambda (x) (/ x sum)) l)))

(define distant-future (seconds->date 2524608000))
(define/contract
  (maximum-points-tied-up
   #:settled-at [settled-at distant-future])
  (->* () (#:settled-at date?) (>=/c 0.0))

  (define days-remaining (date- settled-at (seconds->date (current-seconds))))
  (if (< days-remaining 0)
      0.0

      (let ([v (* 1010.81 (exp (* -0.0107473 days-remaining)))])
	(cond
	 [(> 1000.0 v) 1000.0]
	 [(< v 1.0) 1.0]
	 [else v]))))

(provide normalize-probabilities
	 maximum-points-tied-up)
