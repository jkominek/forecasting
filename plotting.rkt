#lang racket/base

(require plot
	 racket/sequence
	 (file "/home/jkominek/forecasting/questions.rkt")
	 (file "/home/jkominek/forecasting/trading.rkt")
	 (file "/home/jkominek/forecasting/utils.rkt")
	 (file "/home/jkominek/forecasting/opinions.rkt")
	 )

(define
  (generate-choice-renderers
   #:probabilities initial-probabilities
   #:function f)
  
  ; these are all the choices which we'll consider adjusting.
  ; in the binary case, we only need to do one. otherwise we
  ; need to try adjusting all of them
  (define indexes
    (if (= (length initial-probabilities) 2)
        '(1)
        (sequence->list (in-range 0 (length initial-probabilities)))))

  ; loop over every choice, and all the possible new probabilities
  ; for it. compute the function we're maximizing, and check to see
  ; if the new set of probabilities should replace the old
  (for/list ([choice indexes])
    (let ([new-f (lambda (new-probability)
                   (let ([shifted-probabilities
                          (shift-choice-probability
                           initial-probabilities
                           (list choice)
                           (list (exact->inexact new-probability)))])
                     (let ([v (f shifted-probabilities)])
                       (if (void? v)
                           +nan.0
                           v))))])
      (function new-f))))

(define (plot-searches question fs
                       #:user-name my-user-name
                       #:debt-limit [debt-limit (maximum-points-tied-up (question-settlement-at question))]
                       #:beliefs [beliefs (opinion-beliefs (get-opinion (question-id question)))])
  (define renderers
    (for/list ([i (in-naturals)]
               [f fs])
      (line-color (+ 2 i))
      (generate-choice-renderers
       #:probabilities (question-probability question)
       #:function (f #:beliefs beliefs
                     #:assets (question-user-assets question my-user-name)
                     #:initial-probabilities (question-probability question)
                     #:debt-limit debt-limit))))

  (parameterize
      ([plot-x-ticks (ticks (linear-ticks-layout)
                            (lambda (lower upper preticks)
                              (for/list ([pretick preticks])
                                (cat (* 100 (pre-tick-value pretick))))))])
    (for/list ([i (in-range 0 (length (car renderers)))])
      (plot (map (lambda (rs) (list-ref rs i)) renderers)
            #:x-min 1/100
            #:x-max 99/100))))
