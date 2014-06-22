#lang racket

(require net/cookie
         net/http-client
         json
	 (only-in (file "/home/jkominek/forecasting/utils.rkt") lmsr-outcomes))

(define scicast-http-conn (make-parameter #f))
(define cookie-header (make-parameter #f))

(define/contract
  (get-connection)
  (-> http-conn?)

  (unless (scicast-http-conn)
    (scicast-http-conn (http-conn-open "scicast.org" #:ssl? #t)))
  (scicast-http-conn))

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
  ;(printf "~a~n~a~n" status-line headers)
  ;(printf "~a~n" (read-json body-port))
  (let/ec found
    (for ([header headers])
      (when (regexp-match #rx#"^Set-Cookie: session_id" header)
        (let* ([cookie (subbytes header 23)]
               [session-id (car (regexp-match #rx#"^[a-fA-F0-9]+" cookie))])
          (cookie-header (bytes-append #"Cookie: session_id=" session-id))
          (found (void)))))

    (http-conn-close! hc)
    (error "failed to log in or get cookie header")))

;(log-in hc "jkominek" "***REMOVED***")

;(define-values
;  (status-line headers body-port)
;  (http-conn-sendrecv! hc "/users/relevant_activities"
;                       #:method "GET"
;                       #:headers (list (cookie-header)
;                                       "Accept: application/json")))
;(printf "~a~n~a~n" status-line headers)
;(define activities (read-json body-port))
;(write-json activities (open-output-file "activities.json" #:exists 'replace))
;(http-conn-close! hc)

; /trades/create?question_id=459&new_value=0.8099999392500008%2C0.19&dimension=1&interface_type=2&old_values=%5B0.79999995%2C0.19999999%5D&max_allowed_cost=1 HTTP/1.1

(define (n->s v)
  (string-trim #:left? #f #:repeat? #t (real->decimal-string v 20) "0"))

(define/contract
  (make-trade q-id old new #:max-cost [max-cost #f] #:hc [hc (scicast-http-conn)])
  (->* (natural-number/c
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
	(let-values
	    ([(dim diff)
	      (for/fold ([dim 0]
			 [diff (abs (- (car old) (car new)))])
			([o (cdr old)]
			 [n (cdr new)]
			 [idx (in-naturals)])
			(if (> (abs (- o n)) diff)
			    (values idx (abs (- o n)))
			    (values dim diff)))])
	  dim)))

  (define new-value-str (string-join (map n->s new) "%2C"))
  (define old-value-str (string-join (map n->s old) "%2C"))

  (define query
    (format "/trades/create?question_id=~a&new_value=~a&dimension=~a&interface_type=2&old_values=%5B~a%5D&max_allowed_cost=~a"
	    q-id new-value-str dimension old-value-str
	    (n->s (+ 0.0001 (max 0.0 computed-cost)))))

  ;(printf "~a~n" query)
  (define-values
    (status-line headers body-port)
    (http-conn-sendrecv! hc query
                         #:method "GET"
                         #:headers (list "Accept: application/json"
					 (cookie-header))))

  ;(printf "~a~n~a~n" status-line headers)
  (read-json body-port)
  )

(provide get-connection log-in make-trade)
