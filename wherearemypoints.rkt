#lang racket

(require (file "/home/jkominek/forecasting/questions.rkt")
	 (file "/home/jkominek/forecasting/opinions.rkt")
	 (file "/home/jkominek/forecasting/utils.rkt")
         (except-in (planet bzlib/date/plt) date->string)
         (only-in racket/date date->string date-display-format)
	 )

(define my-user-id (make-parameter 296))
(define sort-by (make-parameter 'tied-up))

(void
 (command-line
  #:program "wherearemypoints"
  #:once-each
  [("--uid") user-id
   "User ID to investigate"
   (my-user-id (string->number user-id))]
  [("--resolution")
   "Sort by resolution time"
   (sort-by 'resolution)]
  ))

(question-database
 (load-question-database-url/cache-to-file
  *standard-question-list-url* "data.json" #:max-age 10000))

(define standings (make-hash))
(define expected (make-hash))
(define tied-up (make-hash))

(for ([trade (fetch-user-trades (my-user-id) #:max-age 10000)]
      #:when (have-question? (hash-ref trade 'question_id)))
  (define q-id (hash-ref trade 'question_id))
  (define q (fetch-question q-id))

  (when (and (question-visible? q)
	     (not (question-settled-at q)))
    (define standing
      (map + (hash-ref standings q-id
		       (lambda ()
			 (build-list (length (trade-assets trade)) (lambda x 0.0))))
	   (trade-assets trade)))
    (hash-set! standings q-id standing)
    (hash-set! tied-up q-id (apply min standing))
    ))

(define display-order
  (cond
   [(equal? (sort-by) 'resolution)
    (sort (hash-keys tied-up)
          date>?
          #:key
          (lambda (k) (question-settlement-at (fetch-question k))))]
   [else
    (sort (hash-keys tied-up)
          >
          #:key (lambda (k) (hash-ref tied-up k)))]))

(date-display-format 'iso-8601)

(for/fold
 ([current-date (date->string (question-settlement-at (fetch-question (car display-order))))])
 ([q-id display-order]) 
  (define q (fetch-question q-id))
  (define settlement (question-settlement-at q))
  (define settlements (date->string settlement))
  (define limit (maximum-points-tied-up settlement))

  ;(when (have-opinion? q-id)
  ;  (flsum (map * standing (opinion-beliefs (get-opinion q-id)))))

  (when (equal? (sort-by) 'resolution)
        (unless (equal? current-date settlements)
                (printf "--- ~a~n" settlements)))
  (display
   (string-join
    (list (cat (hash-ref tied-up q-id) 7 2.)
	  (if (< (hash-ref tied-up q-id) limit)
	      "!" " ")
	  " (" (number->string q-id) ") "
	  (string-trim #:repeat? #t (question-name q))
	  "\n")
    ""))

  settlements
  )
