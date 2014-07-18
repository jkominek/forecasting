#lang racket

(require net/cookie
         net/http-client
         json
	 (planet bzlib/date/plt)
	 (file "/home/jkominek/forecasting/questions.rkt")
	 (only-in (file "/home/jkominek/forecasting/utils.rkt") lmsr-outcomes first-value))

(define cookie-header (make-parameter #f))

(define/contract
  (have-session?)
  (-> (or/c #t #f))

  (not (equal? (cookie-header) #f)))

(define/contract
  (get-connection)
  (-> http-conn?)

  (http-conn-open "scicast.org" #:ssl? #t))

(define/contract
  (log-in user-name password #:hc [hc (get-connection)])
  (->* (string? string?) (#:hc http-conn?) void?)

  (define-values
    (status-line headers body-port)
    (http-conn-sendrecv! hc "/session/create"
                         #:method "POST"
                         #:headers (list "Content-Type: application/x-www-form-urlencoded"
                                         "Accept: application/json")
                         #:data (format "username=~a&password=~a" user-name password)))
  (let/ec found
    (for ([header headers])
      (when (regexp-match #rx#"^Set-Cookie: session_id" header)
        (let* ([cookie (subbytes header 23)]
               [session-id (car (regexp-match #rx#"^[a-fA-F0-9]+" cookie))])
          (cookie-header (bytes-append #"Cookie: session_id=" session-id))
          (found (void)))))

    (http-conn-close! hc)
    (error "failed to log in or get cookie header")))

(define (n->s v)
  (string-trim #:left? #f #:repeat? #t (real->decimal-string v 20) "0"))

(define/contract
  (make-trade q-id ordered? old new #:max-cost [max-cost #f] #:hc [hc (get-connection)])
  (->* (natural-number/c
        boolean?
	(non-empty-listof (real-in 0.0 1.0))
	(non-empty-listof (real-in 0.0 1.0)))
       (#:hc http-conn?
	#:max-cost (or/c real? #f))
       jsexpr?)

  (define computed-cost (- (apply min (lmsr-outcomes old new))))
  (when (and (number? max-cost)
	     (> computed-cost max-cost))
    (error "caller miscalculated cost? aborting"))

  (define dimension
    (if (= (length old) 2)
	1
        (+ (if ordered? 1 1)
         (first-value
          (for/fold ([dim 0]
                     [diff (abs (- (car old) (car new)))])
                    ([o (cdr old)]
                     [n (cdr new)]
                     [idx (in-naturals)])
                    (if (> (abs (- o n)) diff)
                        (values idx (abs (- o n)))
                        (values dim diff)))))))

  (define new-value-str (string-join (map n->s new) "%2C"))
  (define old-value-str (string-join (map n->s old) "%2C"))

  (define query
    (format "/trades/create?question_id=~a&new_value=~a&dimension=~a&interface_type=2&old_values=%5B~a%5D&max_allowed_cost=~a"
	    q-id new-value-str dimension old-value-str
	    (n->s (+ 0.0001 (max 0.0 computed-cost)))))

  (define-values
    (status-line headers body-port)
    (http-conn-sendrecv! hc query
                         #:method "GET"
			 #:content-decode '()
                         #:headers (list "Accept: application/json"
					 (cookie-header))))

  (let ([json (read-json body-port)])
    (if (hash-has-key? json 'trade)
	(hash-ref json 'trade)
        (begin
          (write-json json (open-output-file "/tmp/exectrades.log" #:exists 'append))
          #f)))
  )

(define/contract
  (fetch-latest-trades #:hc [hc (get-connection)])
  (->* ()
       (#:hc http-conn?)
       (hash/c natural-number/c jsexpr?))
  (define-values
    (status-line headers body-port)
    (http-conn-sendrecv! hc "/users/relevant_activities?group_by=question"
                         #:method "GET"
                         #:content-decode '()
                         #:headers (list "Accept: application/json"
                                         (cookie-header))))

  ;(printf "~a~n~a~n" status-line headers)

  (define latest (make-hash))

  (with-handlers
   ([exn? (lambda (e) (printf "ACTIVITIES ERROR: ~a~n" e) latest)])

   (define trades (hash-ref (read-json body-port) 'trades))

   (for ([trade trades]
         #:unless (> (length (trade-assumptions trade)) 0))
        (define q-id (hash-ref trade 'question_id))
        (if (hash-has-key? latest q-id)
            (when (date<? (trade-created-at (hash-ref latest q-id))
                          (trade-created-at trade))
                  (hash-set! latest q-id trade))
            (hash-set! latest q-id trade))
        )

   latest))

(provide get-connection log-in make-trade fetch-latest-trades have-session?)
