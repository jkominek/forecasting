#lang racket/base

(require math
	 racket/cmdline
	 racket/function
	 racket/match
	 (only-in racket/list last)
	 racket/async-channel
         (planet bzlib/date/plt)
         (file "/home/jkominek/forecasting/questions.rkt")
         (file "/home/jkominek/forecasting/rankings.rkt")
         (file "/home/jkominek/forecasting/opinions.rkt")
         (file "/home/jkominek/forecasting/utils.rkt")
         (file "/home/jkominek/forecasting/trading.rkt")
         (file "/home/jkominek/forecasting/exectrades.rkt")
         )

(define question-ids (make-parameter #f))
(define my-user-name (make-parameter "jkominek"))
(define forced-debt-limit (make-parameter #f))
(define flip-optimization (make-parameter #f))

(define strategy (make-parameter utility-function))
(define strategy> (make-parameter comparison-function))

(define ramifications (make-parameter #f))
(define exact-stats (make-parameter #f))
(define web-trades (make-parameter #f))

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
  [("--debt-limit") debt-limit
   "Override debt limit; sign is flipped"
   (forced-debt-limit (- (string->number debt-limit)))]
  [("--ignore-debt-limit")
   "Effectively Disable debt limit"
   (forced-debt-limit -1e300)]
  [("--minimize" "--flip")
   "Flip the optimization"
   (flip-optimization #t)]
  [("--web")
   "Offers the opportunity to submit the trade"
   (web-trades #t)]

  #:once-any
  [("--python")
   "Use a duplicate(?) of the final Python rule"
   (strategy python-utility)
   (strategy> comparison-function)]
  [("--curr-score")
   "Maximize current score"
   (strategy (simple-adjustable 'curr-score))
   (strategy> >)]
  [("--final-score")
   "Maximize final score"
   (strategy (simple-adjustable 'final-score))
   (strategy> >)]
  [("--final+assetinc")
   "Maximize final score subject to requiring an asset increase"
   (strategy (simple-adjustable 'final+assetinc))
   (strategy> >)]
  [("--credit")
   "Maximize transaction credit"
   (strategy (simple-adjustable 'credit))
   (strategy> >)]
  [("--debt")
   "Minimize final debt"
   (strategy (simple-adjustable '-debt))
   (strategy> >)]
  [("--final/debt")
   "Maximize expected final score / debt"
   (strategy (simple-adjustable 'final/-debt))
   (strategy> >)]
  [("--total-posassets")
   "Maximize sum of positive assets"
   (strategy (simple-adjustable 'total-positive-assets))
   (strategy> >)]
  [("--total-assets")
   "Maximize sum of assets"
   (strategy (simple-adjustable 'total-assets))
   (strategy> >)]
  [("--exact-stats")
   "Displays exact floats about the question"
   (exact-stats #t)]
  [("--ramifications") choice probability
   "Displays the ramifications of shifting choice to probability"
   (ramifications (cons (string->number choice)
			(exact->inexact (/ (string->number probability) 100))))]
  
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

(define logged-in #f)

(define (perform-opinionated-search question-ids)
  (define considered 0)
  (define displayed 0)

  (current-thread-group (make-thread-group))
  (define finished (make-async-channel))

  (for ([q-id question-ids])
    (thread
     (lambda ()
       (define q (fetch-full-question q-id))
       (define opinion (get-opinion (question-id q)))

       (define trade-sequence
	 (find-optimal-trade-sequence
	  (strategy) (strategy>)
	  #:assets (question-user-assets q (my-user-name))
	  #:debt-limit (if (number? (forced-debt-limit))
			   (forced-debt-limit)
			   (* (opinion-strength opinion)
			      (maximum-points-tied-up (question-settlement-at q))))
	  #:minimum-change 1/4
	  #:beliefs (opinion-beliefs opinion)
	  #:initial-probabilities (question-probability q)
	  #:trade-limit 5
	  ))

       ; if the thread doesn't put something, we'll have a problem later
       (async-channel-put finished (list q-id q opinion trade-sequence)))))

  (for ([ignore question-ids])
    (match-define
     (list q-id q opinion trade-sequence)
     (async-channel-get finished))

    (set! considered (add1 considered))    
    (when (and (equal? trade-sequence '())
	       (not *bulk*))
      (printf "nothing to do on ~a~n" q-id))

    (when (> (length trade-sequence) 0)
      (define summary-details (make-hash))
      (define summary
	(summarize-effect-of-trades
	 q trade-sequence
	 #:user-name (my-user-name)
	 #:summary-hash summary-details
	 #:debt-limit (* (opinion-strength opinion)
			 (maximum-points-tied-up (question-settlement-at q)))
	 #:beliefs (opinion-beliefs opinion)))
      (when (or (> (hash-ref summary-details 'credit) 2.0)
		(> (hash-ref summary-details 'current-score-improvement)
		   (max 10.0
			(* 1/10 (hash-ref summary-details 'initial-current-score))))
		(> (hash-ref summary-details 'total-Δassets) 25)
		(if (hash-has-key? summary-details 'final-score-improvement)
		    (or (and (< (hash-ref summary-details 'initial-final-score) 10.0)
			     (> (hash-ref summary-details 'final-score-improvement) 1.0))
			(and (>= (hash-ref summary-details 'initial-final-score) 10.0)
			     (> (hash-ref summary-details 'final-score-improvement)
				(* 1/10 (hash-ref summary-details 'initial-final-score)))))
		    #f))
	(set! displayed (add1 displayed))
	(printf "(~a) ~a~n~ https://scicast.org/#!/questions/~a/trades/create/power~n~a~n"
		(question-id q)
		(question-name q)
		(question-id q)
		summary)

	(when (web-trades)
	  (printf "execute trade? ")
	  (when (equal? (string-ref (read-line) 0) #\y)
	    (unless logged-in
	      (log-in "jkominek" "***REMOVED***"))
	    (set! logged-in #t)
	    (make-trade q-id (question-probability q)
			(last trade-sequence))))
	))
    ) ; for

  (when (= 0 displayed)
    (printf "~a questions considered~n" considered)))

(define (display-ramifications question-ids choice probability)
  (for ([q-id (in-list question-ids)])
    (define q (fetch-full-question q-id))
    (define opinion (get-opinion q-id))
    (define new-prob (shift-choice-probability (question-probability q)
					       choice
					       probability))
    (printf "(~a) ~a~n~ https://scicast.org/#!/questions/~a/trades/create/power~n~a~n"
	    (question-id q)
	    (question-name q)
	    (question-id q)
	    (summarize-effect-of-trades
	     q (list new-prob)
	     #:user-name (my-user-name)
	     #:debt-limit (* (opinion-strength opinion)
			     (maximum-points-tied-up (question-settlement-at q)))
	     #:beliefs
	     (if (have-opinion? q-id)
		 (opinion-beliefs opinion)
		 #f)))))

(define (display-stats question-ids)
  (for ([q-id (in-list question-ids)])
    (define q (fetch-full-question q-id))
    (printf "~a~n" (question-probability q))
    ))

(cond
 [(exact-stats)
  (display-stats (question-ids))]

 ; user told us exactly what choice to set to what value
 [(pair? (ramifications))
  (let ([choice (car (ramifications))]
	[probability (cdr (ramifications))])
    (display-ramifications (question-ids) choice probability))]

 [else
  (perform-opinionated-search (filter have-opinion? (question-ids)))]

 )
