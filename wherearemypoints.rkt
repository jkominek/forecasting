#lang racket

(require (file "/home/jkominek/forecasting/questions.rkt")
	 (file "/home/jkominek/forecasting/opinions.rkt")
	 (file "/home/jkominek/forecasting/utils.rkt")
	 )

(define my-user-id (make-parameter 296))

(void
 (command-line
  #:program "wherearemypoints"
  #:once-each
  [("--uid") user-id
   "User ID to investigate"
   (my-user-id (string->number user-id))]
  ))

(question-database
 (load-question-database-url/cache-to-file
  *standard-question-list-url* "data.json"))

(define standings (make-hash))
(define expected (make-hash))
(define tied-up (make-hash))

(for ([trade (fetch-user-trades (my-user-id))]
      #:when (have-question? (hash-ref trade 'question_id)))
  (define q-id (hash-ref trade 'question_id))
  (printf "~a~n" q-id)
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

(for ([q-id (sort (hash-keys tied-up)
		  > ; reverse
		  #:key (lambda (k) (hash-ref tied-up k)))])
  (define q (fetch-question q-id))
  (define limit (maximum-points-tied-up (question-settlement-at q)))

  ;(when (have-opinion? q-id)
  ;  (apply + (map * standing (opinion-beliefs (get-opinion q-id)))))

  (display
   (string-join
    (list (cat (hash-ref tied-up q-id) 7 2.)
	  (if (< (hash-ref tied-up q-id) limit)
	      "!" " ")
	  " (" (number->string q-id) ") "
	  (string-trim #:repeat? #t (question-name q))
	  "\n")
    ""))
  )
