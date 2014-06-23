#lang racket/base

(require math
	 (planet bzlib/date/plt)
	 racket/contract
	 (only-in srfi/54 cat)
	 net/url
	 racket/port
	 file/gunzip)

(provide cat)

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
   [settled-at distant-future])
  (->* ((or/c date? #f)) () (<=/c 0.0))

  (if settled-at
      (let ([days-remaining
	     (date- settled-at (seconds->date (current-seconds)))])
	(if (< days-remaining 0)
	    0.0
	    (let ([v (* 1010.81 (exp (* -0.0107473 days-remaining)))])
	      (- (cond
		  [(> v 1000.0) 1000.0]
		  [(< v 1.0) 5.0]
		  [else v])))))
      -1.0))

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

(define/contract
  (shift-choice-probability probabilities choice new-value)
  (-> (listof (real-in 0.0 1.0))
      natural-number/c
      (real-in 0 1)
      (listof (real-in 0.0 1.0)))

  (define leftover (- (list-ref probabilities choice) new-value))
  (define sum-of-unspecified-probabilies
    (for/fold ([sum 0.0])
	      ([p probabilities]
	       [i (in-naturals)])
	      (if (= i choice)
		  sum
		  (+ sum p))))
  (define l/s (/ leftover sum-of-unspecified-probabilies))

  (for/list ([p probabilities]
             [i (in-naturals)])
    (if (= i choice)
        new-value
        (if (> sum-of-unspecified-probabilies 0.0)
            (+ p (* l/s p))
            0.0))))

(define rate-limiter (make-channel))
(void (thread
 (lambda ()
   (for ([i (in-naturals)])
     (channel-put rate-limiter i)
     (sleep 1)))))

(define (get-gzip-pure-port url-string)
  ; prevents us from hitting the web site very hard
  (channel-get rate-limiter)
  (printf "fetching from network!~n")

  (define p (get-pure-port (string->url url-string)
			   '("Accept-encoding: gzip")))
  (define-values (in out) (make-pipe))
  (thread (lambda ()
	    (gunzip-through-ports p out)
	    (flush-output out)
	    (close-output-port out)))
  in)

(define (open-url/cache-to-file url path #:max-age [max-age 1200])
  (let ([last-modification
	 (file-or-directory-modify-seconds path #f (lambda () 0))])
    (when (> (- (current-seconds) last-modification) max-age)
      (let ([dest-port (open-output-file path #:exists 'replace)])
	(copy-port (get-gzip-pure-port url) dest-port)
	(flush-output dest-port)
	(close-output-port dest-port)))
    (open-input-file path)))

(define (eat-up-everything)
  (let/ec done
    (for ([x (in-naturals)])
         (define v (sync/timeout 0.1 (current-input-port)))
         (if (input-port? v)
             (read-byte v)
             (done (void))))))
  
(provide normalize-probabilities
	 maximum-points-tied-up
         lmsr-outcome
         lmsr-outcomes
         shift-choice-probability
	 open-url/cache-to-file
	 eat-up-everything
	 )
