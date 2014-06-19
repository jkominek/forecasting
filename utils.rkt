#lang racket/base

(require math
	 (planet bzlib/date/plt)
	 racket/contract)

; Takes a list of relative likelihoods, and normalizes
; them all into probabilities.
(define/contract
  (normalize-probabilities l)
  (-> (listof (>=/c 0)) (listof (real-in 0 1)))

  (let ([sum (apply + l)])
    (map (lambda (x) (/ x sum)) l)))

(define distant-future (seconds->date 2524608000))
; Given a time, compute the number of points we're willing to
; sink into a question settling at that date. Exponential decay
; thing.
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

(define log2 (log 2))
; Robin Hanson's LMSR formula, but with the probability
; division done in log-space, so things don't go horribly
; wrong.
(define/contract
  (lmsr-outcome start stop)
  (-> (real-in 0.0 1.0) (real-in 0.0 1.0) real?)
  
  (* -100.0
     (- (/ (log start) log2)
        (/ (log stop) log2))))

; Applies the LMSR to a list of old probabilities and new
; ones, providing the per-choice cost as a list.
(define/contract
  (lmsr-outcomes start stop)
  (-> (listof (real-in 0.0 1.0))
      (listof (real-in 0.0 1.0))
      (listof real?))
  
  (map lmsr-outcome start stop))

(provide normalize-probabilities
	 maximum-points-tied-up
         lmsr-outcome
         lmsr-outcomes)
