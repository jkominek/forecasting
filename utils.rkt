#lang racket/base

(require math
         math/flonum
	 (planet bzlib/date/plt)
	 racket/contract
	 (rename-in (only-in srfi/54 cat) [cat unsafe-cat])
	 net/url
	 racket/port
	 racket/string
	 file/gunzip
         syntax/parse/define)

(define cat
  (lambda x
    (cond
     [(not (real? (car x))) (apply unsafe-cat x)]
     [(infinite? (car x)) (format "~a" (car x))]
     [(nan? (car x)) (format "~a" (car x))]
     [else (apply unsafe-cat x)])))

(provide cat)

(define-simple-macro (first-value body:expr)
  (call-with-values
   (lambda () body)
   (lambda x (car x))))

(provide first-value)

; Takes a list of relative likelihoods, and normalizes
; them all into probabilities.
(define/contract
  (normalize-probabilities l)
  (-> (listof (>=/c 0)) (listof (real-in 0 1)))

  (let* ([l (map exact->inexact l)]
         [sum (flsum l)])
    (map (lambda (x) (fl/ x sum)) l)))

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
	     (exact->inexact
              (date- settled-at (seconds->date (current-seconds))))])
	(if (< days-remaining 0.0)
	    0.0
	    (let ([v (fl* (fl* 0.333 1010.81)
                          (flexp (fl* (fl* -0.0107473 1.0) days-remaining)))])
	      (fl- 0.0
                   (cond
                    [(fl> v 1000.0) 1000.0]
                    [(fl< v    7.5)    7.5]
                    [else v])))))
      -1.0))

(define log2 (fllog 2.0))
; Robin Hanson's LMSR formula, but with the probability
; division done in log-space, so things don't go horribly
; wrong.
(define/contract
  (lmsr-outcome start stop)
  (-> (real-in 0.0 1.0) (real-in 0.0 1.0) real?)
  
  (fl* -100.0
       (fl- (fl/ (fllog start) log2)
            (fl/ (fllog stop) log2))))

; Applies the LMSR to a list of old probabilities and new
; ones, providing the per-choice cost as a list.
(define/contract
  (lmsr-outcomes start stop)
  (-> (listof (real-in 0.0 1.0))
      (listof (real-in 0.0 1.0))
      (listof real?))
  
  (map lmsr-outcome start stop))

(define (index-list-lookup i idxs vals)
  (if (= i (car idxs))
      (car vals)
      (index-list-lookup i (cdr idxs) (cdr vals))))

(define/contract
  (shift-choice-probability probabilities choice raw-new-values)
  (-> (listof (real-in 0.0 1.0))
      (listof natural-number/c)
      (listof (real-in 0 1))
      (listof (real-in 0.0 1.0)))

  (define new-value
    (for/list ([i (in-range 0 (length probabilities))])
      (if (member i choice)
          (index-list-lookup i choice raw-new-values)
          (void))))

  (define leftover
    (for/sum ([i (in-naturals)]
              [p probabilities]
              [n new-value]
              #:when (member i choice))
       (- p n)))

  (define sum-of-unspecified-probabilies
    (for/sum ([i (in-naturals)]
              [p probabilities]
              #:when (not (member i choice)))
      p))

  (define l/s (fl/ leftover sum-of-unspecified-probabilies))

  (for/list ([p probabilities]
             [n new-value]
             [i (in-naturals)])
    (if (member i choice)
        n
        (if (fl> sum-of-unspecified-probabilies 0.0)
            (fl+ p (fl* l/s p))
            0.0)))
  )

(define rate-limiter (make-channel))
(void (thread
 (lambda ()
   (channel-put rate-limiter 'startup)
   (for ([i (in-naturals)])
     (channel-put rate-limiter i)
     (sleep 7.5)))))
(define (rate-limit)
  (void (channel-get rate-limiter)))

(define (get-gzip-pure-port url-string)
  ; prevents us from hitting the web site very hard
  (rate-limit)
  (printf "fetching from network!~n")

  (time
   (define p (get-pure-port (string->url url-string)
                            '("Accept-encoding: gzip")))
   (define-values (in out) (make-pipe))
   (thread (lambda ()
             (gunzip-through-ports p out)
             (flush-output out)
             (close-output-port out)))
   in))

(define (open-url/cache-to-file url path #:max-age [max-age 1800])
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

(define (pretty-probability-list l)
  (format "[~a ]" (string-join (map (lambda (v) (cat (* 100 v) 5 1. 'inexact)) l))))
(define (pretty-asset-list l)
  (format "[~a ]"
	  (string-join
	   (map (lambda (v)
		  (if (rational? v)
		      (cat (exact-round v) 5)
		      (cat (format "~a" v) 5)))
		l))
	  ))
(define (pretty-string-list l)
  (format "[~a ]" (string-join l)))

(define (update-standings q-id assets #:standings standings)
  (hash-update! standings q-id
                (lambda (orig)
                  (map + orig assets))
                (lambda ()
                  (build-list (length assets) (lambda x 0.0)))))
  
(provide normalize-probabilities
	 maximum-points-tied-up
         lmsr-outcome
         lmsr-outcomes
         shift-choice-probability
	 open-url/cache-to-file
	 eat-up-everything
	 pretty-probability-list
	 pretty-asset-list
	 pretty-string-list
         update-standings
         rate-limit
	 )
