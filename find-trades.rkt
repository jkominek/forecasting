#lang racket/base

(require math
	 racket/cmdline
	 racket/function
	 racket/promise
         (planet bzlib/date/plt)
         (file "/home/jkominek/forecasting/questions.rkt")
         (file "/home/jkominek/forecasting/rankings.rkt")
         (file "/home/jkominek/forecasting/opinions.rkt")
         (file "/home/jkominek/forecasting/utils.rkt")
         (file "/home/jkominek/forecasting/trading.rkt")
         )

(define question-ids (make-parameter #f))
(define my-user-name (make-parameter "jkominek"))
(define forced-debt-limit (make-parameter #f))
(define flip-optimization (make-parameter #f))

(define strategy (make-parameter utility-function))
(define strategy> (make-parameter comparison-function))

(define ramifications (make-parameter #f))

(question-database
 (load-question-database-url/cache-to-file
  *standard-question-list-url* "data.json"))

(void
 (command-line
  #:program "find-trades"
  #:once-each
  [("-u" "--user") user
                   "Username to trade for"
                   (my-user-name user)]
  [("--debt") debt-limit
              "Override debt limit; sign is flipped"
              (forced-debt-limit (- (string->number debt-limit)))]
  [("--minimize" "--flip") "Flip the optimization"
                           (flip-optimization #t)]

  #:once-any
  [("--curr-score") "Maximize current score"
                    (strategy (simple-adjustable 'curr-score))
                    (strategy> >)]
  [("--final-score") "Maximize final score"
                     (strategy (simple-adjustable 'final-score))
                     (strategy> >)]
  [("--final+assetinc") "Maximize final score subject to requiring an asset increase"
                        (strategy (simple-adjustable 'final+assetinc))
                        (strategy> >)]
  [("--credit") "Maximize transaction credit"
                (strategy (simple-adjustable 'credit))
                (strategy> >)]
  [("--debt") "Minimize final debt"
              (strategy (simple-adjustable '-debt))
              (strategy> >)]
  [("--final/debt") "Maximize expected final score / debt"
                    (strategy (simple-adjustable 'final/-debt))
                    (strategy> >)]
  [("--total-posassets") "Maximize sum of positive assets"
                         (strategy (simple-adjustable 'total-positive-assets))
                         (strategy> >)]
  [("--total-assets") "Maximize sum of assets"
                      (strategy (simple-adjustable 'total-assets))
                      (strategy> >)]
  [("--ramifications") choice probability
                       "Displays the ramifications of shifting choice to probability"
		       (ramifications (cons (string->number choice)
					    (/ (string->number probability) 100)))]
  
  #:args raw-question-ids
  (question-ids (map string->number raw-question-ids))
  ))

(define *bulk* #f)
(when (null? (question-ids))
  (question-ids (all-question-ids))
  (set! *bulk* #t)
  )

(when (flip-optimization)
  (strategy> (negate (strategy>))))

(define (perform-opinionated-search question-ids)
  (define considered 0)
  (define displayed 0)

  (define trade-sequences
    (for/list ([q-id question-ids])
      (delay/idle
	(define q (fetch-full-question q-id))

	(define trade-sequence
	  (find-optimal-trade-sequence
	   (strategy) (strategy>)
	   #:assets (question-user-assets q (my-user-name))
	   #:debt-limit (if (number? (forced-debt-limit))
			    (forced-debt-limit)
			    (maximum-points-tied-up (question-settlement-at q)))
	   #:beliefs (opinion-beliefs (get-opinion (question-id q)))
	   #:initial-probabilities (question-probability q)
	   #:trade-limit 5
	   ))

	(values q-id q trade-sequence))))

  (for ([promise (in-list trade-sequences)])
    (define-values
      (q-id q trade-sequence)
      (force promise))

    (set! considered (add1 considered))    
    (when (and (equal? trade-sequence '())
	       (not *bulk*))
      (printf "nothing to do on ~a~n" q-id))

    (when (> (length trade-sequence) 0)
      (set! displayed (add1 displayed))
      (printf "(~a) ~a~n~a~n"
	      (question-id q)
	      (question-name q)
	      (summarize-effect-of-trades
	       q trade-sequence
	       #:user-name (my-user-name)
	       #:beliefs (opinion-beliefs (get-opinion (question-id q))))))
    )
  (when (= 0 displayed)
    (printf "~a questions considered~n" considered)))

(define (display-ramifications question-ids choice probability)
  (for ([q-id (in-list question-ids)])
    (define q (fetch-full-question q-id))
    (define new-prob (shift-choice-probability (question-probability q)
					       choice
					       probability))
    (printf "(~a) ~a~n~a~n"
	    (question-id q)
	    (question-name q)
	    (summarize-effect-of-trades
	     q (list new-prob)
	     #:user-name (my-user-name)
	     #:beliefs
	     (if (have-opinion? q-id)
		 (opinion-beliefs (get-opinion q-id))
		 #f)))))

(cond

 ; user told us exactly what choice to set to what value
 [(pair? (ramifications))
  (let ([choice (car (ramifications))]
	[probability (cdr (ramifications))])
    (display-ramifications (question-ids) choice probability))]

 [else
  (perform-opinionated-search (filter have-opinion? (question-ids)))])
