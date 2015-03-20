#lang racket/base

(require math
	 racket/cmdline
	 racket/function
	 racket/match
	 racket/set
         racket/string
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
(define my-user-id (make-parameter 296))
(define forced-debt-limit (make-parameter #f))
(define flip-optimization (make-parameter #f))

(define strategy (make-parameter kelly-utility))
(define strategy> (make-parameter >))

(define ramifications (make-parameter #f))
(define exact-stats (make-parameter #f))
(define web-trades (make-parameter #f))
(define monitor (make-parameter #f))
(define annealing-search (make-parameter #f))

(define verbose (make-parameter #f))

(question-database
 (load-question-database-url/cache-to-file  *standard-question-list-url* "data.json"))

(void
 (command-line
  #:program "find-trades"
  #:once-each
;  [("-u" "--user") user
;   "User ID to trade for"
;   (my-user-id (string->number user))]
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
  [("--monitor")
   "Monitor on going activity for trade opportunities"
   (monitor #t)]
  [("--anneal")
   "Uses simulated annealing to find trade"
   (annealing-search #t)]
  [("-v")
   "Verbose"
   (verbose #t)]
  [("--safe")
   "Safe opinions only"
   (include-unsafe #f)]

  #:once-any
  [("--kelly")
   "Use Kelly betting"
   (strategy kelly-utility)
   (strategy> >)]
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
   (ramifications (cons (map string->number (string-split choice ","))
                        (map (lambda (p)
                               (exact->inexact (/ (string->number p) 100)))
                             (string-split probability ","))))]
  
  #:args raw-question-ids
  (question-ids (map string->number raw-question-ids))
  ))

(when (null? (question-ids))
  (question-ids (all-question-ids)))

(when (flip-optimization)
  (strategy> (negate (strategy>))))

(define standings (make-hash))

(for ([trade (fetch-user-trades (my-user-id))])
  (define q-id (hash-ref trade 'question_id))
  (update-standings q-id (trade-assets trade)
                    #:standings standings))

(define processed-up-to (make-hash))
(define (seen-question-to q-id d)
  (if (hash-has-key? processed-up-to q-id)
      (let ([prev (hash-ref processed-up-to q-id)])
	(if (date<? prev d)
	    (begin
	      (hash-set! processed-up-to q-id d)
	      #t)
	    #f))
      (begin
	(hash-set! processed-up-to q-id d)
	#t)))

(define threads (mutable-set))
(define ready-trades (make-async-channel))

(define (empty-assets q)
  (build-list (length (question-probability q)) (lambda x 0.0)))

(define *RATE* 0.00009)

(define (compute-present-value-factors q o)
  (let* ([s (opinion-settlement o)]
         [now (current-seconds)]
         [settlement-at (question-settlement-at q)]
         [final (if settlement-at
                    (date->seconds settlement-at)
                    1e300)])
    (map
     (lambda (v)
       (cond
        [(<= v 0.0) 0.0]
        [(>= v 1.0) 1.0]
        [else v]))
     (if s
         (for/list ([a-s s])
           (cond
            [(<= a-s now) 1.0]
            [(< final a-s) (/ (exp (* *RATE* (/ (- final now) 86400.0))))]
            [else (/ (exp (* *RATE* (/ (- a-s now) 86400.0))))]))
         (for/list ([i (question-probability q)])
                   ;1.0
                   ; uncomment this when everything has settlement info.
                   ; until then, no point disturbing existing assets
                   (/ (exp (* *RATE* (/ (- final now) 86400.0))))
                   )))))

(define (perform-opinionated-search question-ids)
  (for ([q-id question-ids])
    (set-add! threads
      (thread
       (lambda ()
	 (define q (fetch-question q-id))
	 (define opinion (get-opinion (question-id q)))

         ;(define search-step
         ;  (let ([m (question-serialized-model q)])
         ;    (if (hash? m)
         ;        (let ([range (hash-ref m 'range)]
         ;              [stepSize (string->number (hash-ref m 'stepSize))])
         ;          (/ (- (cadr range) (car range)) stepSize))
         ;        1)))

	 (seen-question-to q-id (question-updated-at q))

	 (define trade-sequence
           ;(time
            (
             (if (annealing-search)
                 search-optimal-trade
                 find-optimal-trade-sequence)
             (strategy) (strategy>)
             #:assets (hash-ref standings q-id (empty-assets q))
             #:debt-limit (if (number? (forced-debt-limit))
                              (forced-debt-limit)
                              (* (opinion-strength opinion)
                                 (maximum-points-tied-up (question-settlement-at q))))
             #:minimum-change 1/4
             #:beliefs (opinion-beliefs opinion)
             #:initial-probabilities (question-probability q)
             #:trade-limit 8
             #:locked (question-choices-locked q)
             #:present-value-factors (compute-present-value-factors q opinion)
             ));)

	 (when (> (length trade-sequence) 0)
	   (async-channel-put ready-trades
			      (list q-id q opinion trade-sequence)))))))
  )

(define seen-self-trades (mutable-set))

(define (start-monitoring [delay-chunk 4])
  (sleep 120)
  (printf "checking ~a~n" (date->string (current-date)))
  (define trades (fetch-latest-trades))
  (define found-stuff #f)
;  (for ([trade (in-hash-values trades)]
;        #:when (equal? (my-user-id) (hash-ref trade 'user_id))
;        #:unless (set-member? seen-self-trades (trade-id trade)))
;    (printf "saw self trade ~a on ~a~n" (trade-id trade)
;            (question-name (trade-question trade))))

  (for ([trade (in-hash-values trades)]
	#:when (and (not (equal? (my-user-id) (hash-ref trade 'user_id)))
		    (have-opinion? (hash-ref trade 'question_id))
		    (seen-question-to (hash-ref trade 'question_id) (trade-created-at trade))))
    (set! found-stuff #t)
    (printf "~a traded on ~a~n"
	    (user-name (trade-user trade))
	    (question-name (fetch-question (hash-ref trade 'question_id))))

    (define q-id (hash-ref trade 'question_id))
    (define q (hash-set (fetch-question q-id) 'prob (trade-new-values trade)))
    (update-question q-id q)
    (define opinion (get-opinion q-id))

    (define trade-sequence
      (find-optimal-trade-sequence
       (strategy) (strategy>)
       #:assets (hash-ref standings q-id (empty-assets q))
       #:debt-limit (if (number? (forced-debt-limit))
			(forced-debt-limit)
			(* (opinion-strength opinion)
			   (maximum-points-tied-up (question-settlement-at q))))
       #:minimum-change 1/4
       #:beliefs (opinion-beliefs opinion)
       #:initial-probabilities (trade-new-values trade)
       #:present-value-factors (compute-present-value-factors q opinion)
       #:locked (question-choices-locked q)
       #:trade-limit 5
       ))

    (when (> (length trade-sequence) 0)
      (async-channel-put ready-trades
			 (list q-id q opinion trade-sequence)))
    )
  (flush-output)
  (if found-stuff
      (begin
        (sleep (+ 120 (random 120)))
        (start-monitoring))
      (begin
        (sleep
         (min 1800
          (+ 90 (* delay-chunk 1/4) (random (round (* 3/4 delay-chunk))))))
        (monitoring-comprehensive (+ 120 delay-chunk))))
  )

(define last-comprehensive-search (current-seconds))

(define (monitoring-comprehensive delay-chunk)
  ; check to see if we should refetch all questions and do a big search
;  (when (> (- (current-seconds) last-comprehensive-search)
;           (* 30 60))
;    (question-database
;     (load-question-database-url/cache-to-file  *standard-question-list-url* "data.json"))
;    (perform-opinionated-search (filter have-opinion? (question-ids))))

  (start-monitoring delay-chunk)
  )

(define (display-ramifications question-ids choice probability)
  (for ([q-id (in-list question-ids)])
    (set-add! threads
      (thread (lambda ()
	(define q (fetch-full-question q-id))
	(define opinion (get-opinion q-id))
	(define new-prob (shift-choice-probability (question-probability q)
						   choice
						   probability))
	(async-channel-put ready-trades
			   (list q-id q opinion (list new-prob)))
	)))
    (sleep 0.05)))

(define (display-stats question-ids)
  (for ([q-id (in-list question-ids)])
    (define q (fetch-full-question q-id))
    (printf "~a~n" (question-probability q))
    ))

(when (monitor)
  (printf "starting monitor thread~n")
  (set-add! threads (thread start-monitoring)))

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

; display all the trades (and maybe execute them) that the previous
; stuff is generating in the background
(let/ec finished
  (define seen 0)
  (for ([ignore (in-naturals)])
    (let/ec continue
      (define from-channel
	(sync/timeout 0.5 ready-trades))

      (unless from-channel
	; nothing showed up, are we out of work?
	(define live-count
	  (for/fold ([live 0])
		    ([thread (in-set threads)])
		    (if (thread-running? thread)
			(add1 live)
			live)))
	(if (= 0 live-count)
	    (finished (void))
	    (continue (void))))

      (set! seen (add1 seen))

      (match-define
       (list q-id q opinion trade-sequence)
       from-channel)

      ;(printf "got ~a in on the channel. ~a long trade seq~n" q-id (length trade-sequence))

      (define summary-details (make-hash))
      (printf "   considering ~a shift to ~a~n" q-id
              (pretty-probability-list (last trade-sequence)))

      (flush-output)

      (define summary
	(summarize-effect-of-trades
	 q
	 trade-sequence
	 #:initial-assets (hash-ref standings q-id (empty-assets q))
	 #:user-name (my-user-name)
	 #:summary-hash summary-details
	 #:debt-limit (* (opinion-strength opinion)
			 (maximum-points-tied-up (question-settlement-at q)))
	 #:beliefs (opinion-beliefs opinion)
         #:present-value-factors (compute-present-value-factors q opinion)
	 ))

      ; The next three blocks check to see if the proposed trade is high
      ; enough quality to be worth proposing to the user / executing.
      ; First step, define all the checks that are always available to us.
      (define potential-improvements
        `(#;(credit
           ,(hash-ref summary-details 'credit)
           ,(max 20.0
                 (* 1/200 (apply min (hash-ref standings q-id (empty-assets q))))))
          #;(current-score
           ,(hash-ref summary-details 'current-score-improvement)
           ,(max 50.0
                 (* 1/10 (hash-ref summary-details 'initial-current-score))))
          #;(total-assets
           ,(hash-ref summary-details 'total-Δassets)
           500)
          ))

      ; Next, if we have specific beliefs about this one, we've got some
      ; more checks we can do to see if the trade is worthwhile
      (when (hash-has-key? summary-details 'final-score-improvement)
            #;(set! potential-improvements
                  (cons
                   `(combined
                     ,(hash-ref summary-details 'kelly-improvement)
                     ,(if
                       (and (> (hash-ref summary-details 'final-score-improvement) 0.0)
                            (> (hash-ref summary-details 'current-score-improvement) 0.0)
                            (> (hash-ref summary-details 'credit) 0.0))
                       0.0 1.0))
                   potential-improvements))
            (set! potential-improvements
                  (cons
                   `(kelly-improvement
                     ,(hash-ref summary-details 'kelly-improvement)
                     ,(if (< (hash-ref summary-details 'final-score-improvement) -10)
                          (min 0.06 (* 15/100 (hash-ref summary-details 'initial-kelly)))
                          (if (> (hash-ref summary-details 'credit) 0.0)
                              (min 0.01 (* 1/100 (hash-ref summary-details 'initial-kelly)))
                              (min 0.05
                                   (* 1/10 (hash-ref summary-details 'initial-kelly))))))
                   potential-improvements))
            (set! potential-improvements
                  (cons
                   `(final-score
                     ,(hash-ref summary-details 'final-score-improvement)
                     ,(min 10.0
                           (max 1.0 (* 10/100 (max 1.0 (hash-ref summary-details 'initial-final-score))))))
                   potential-improvements)))

      ; Finally
      (unless
       ; Look to see if any of the attributes are sufficient
       (for/fold ([sufficient-improvement? (or (verbose) (ramifications))])
                 ([thing potential-improvements])
          (match-let ([(list identifier value target)
                       thing])
            (if (or (>= value target)
                    sufficient-improvement?
                    ;(<= (random) (/ value target))
                    )
                #t
                (begin
                  ; complain about the ones that aren't
                  (when (>= value (/ target 2))
                    (printf "     ~a was ~a, missing ~a~n" identifier (cat value 1 2.) (cat target 1 2. 'inexact)))
                  sufficient-improvement?))))
        ; bail out of this loop if we didn't find anything to justify the trade
        (continue (void)))

      ; display the details on the trade
      (printf "(~a) ~a~n~ http://scicast.org/#!/questions/~a/trades/create/power~n~a~n"
	      (question-id q)
	      (question-name q)
	      (question-id q)
	      summary)

      (unless (web-trades)
	(continue (void)))

      (unless #t ;(= 728 q-id)
        ; before prompting the user, read any crap
        ; still in the current-input buffers, so we
        ; can't be confused about which trade they
        ; want us to execute
        (eat-up-everything)

	(printf "execute trade? ")
	(unless (regexp-match #rx"^y" (read-line))
          ; anything starting with 'y' is good
	  (continue (void))))

      (let ([trade
	     (make-trade q-id
                         (question-ordered? q)
                         (question-probability q)
			 (last trade-sequence))])
	(if trade
            ; got a good response back, trade successful
	    (let ([blank-standings (build-list (length (trade-assets trade)) (lambda x 0.0))])
	      (printf "trade successful~n")
	      (printf "standings were@ ~a"
                      (pretty-asset-list (hash-ref standings q-id blank-standings)))
              ; update our memory of our current asset standings
	      (update-standings q-id (trade-assets trade)
                                #:standings standings)
	      (printf " now@ ~a~n"
                      (pretty-asset-list (hash-ref standings q-id)))
	      )
            ; didn't get a good response, assume the trade failed
	    (printf "failed to make trade~n")))
      ))) ; for, let/ec

(for ([thread threads])
  (thread-wait thread))
