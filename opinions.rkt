#lang racket/base

(require (planet bzlib/date/plt)
	 (file "/home/jkominek/forecasting/utils.rkt"))

(define opinion-database
  (make-parameter (make-hash)))

(define (load-opinion-database path)
  (read (open-input-file path)))

(define (have-opinion? id #:opinion-database [opinion-database (opinion-database)])
  (hash-has-key? opinion-database id))

(define (get-opinion id #:opinion-database [opinion-database (opinion-database)])
  (hash-ref opinion-database id))

(provide opinion-database load-opinion-database have-opinion? get-opinion)

(struct opinion [for-id beliefs strength]
	#:constructor-name make-opinion)

(define (wrapped-opinion-beliefs o)
  (let ([v (opinion-beliefs o)])
    (normalize-probabilities
     (if (procedure? v)
	 (v)
	 v))))

(define (wrapped-opinion-strength o)
  (let ([v (opinion-strength o)])
    (if (procedure? v)
	(v)
	v)))

(provide (rename-out [wrapped-opinion-beliefs opinion-beliefs]
		     [wrapped-opinion-strength opinion-strength]))

(define (o id beliefs [strength 1.0])
  (hash-set! (opinion-database)
	     id
	     (make-opinion
	      id
	      (if (procedure? beliefs)
		  beliefs
		  (lambda () beliefs))
	      (if (procedure? strength)
		  strength
		  (lambda () strength)))))

(define weak 0.5)
(define strong 2.0)

(define (yes p)
  (let ([100-p (- 100 p)])
    (lambda ()
      (list 100-p p))))

(define (no p)
  (let ([100-p (- 100 p)])
    (lambda ()
      (list p 100-p))))

(define (goes-to f initial
		 #:start start #:stop stop
		 #:flat-before [flat-before #t])
  (let ([start (date->seconds
		(if (string? start)
		    (read-iso8601 start)
		    start))]
	[stop  (date->seconds
		(if (string? stop)
		    (read-iso8601 stop)
		    stop))])
    (lambda ()
      (if (and flat-before
	       (< (current-seconds) start))
	  initial
	  (let ([scale (/ (- (current-seconds) start)
			  (- stop start))])
	    (f (+ (* (- 100 initial) scale) initial)))))))

(define (varying-strength start stop
			  #:initial-strength [initial weak]
			  #:final-strength [final strong])
  (let ([start (date->seconds
		(if (string? start)
		    (read-iso8601 start)
		    start))]
	[stop  (date->seconds
		(if (string? stop)
		    (read-iso8601 stop)
		    stop))])
    (lambda ()
      (let ([scale (/ (- (current-seconds) start)
		      (- stop start))])
	(+ (* (- final initial) scale) initial)))))

; When will the IPv6 traffic measured by Google surpass 4%?
(o 344 '(0 0 0 0.997 0.003 0))

; what happened to mh370
(o 365 '(0 0 0 0 0 100) (* 5 strong))

; Will Nature retract one or more of the January 2014 papers by H. Obokata et al. describing stimulus-triggered acquisition of pluripotency (STAP)?
(o 395 (yes 70) weak)

; Will scientists create a fully air-transmissible, mammalian-infectious strain of avian influenza in a laboratory setting by the end of 2014?
(o 656 '(1 2) 0.08)

; Will Jupiter's Great Red Spot shrink below 9,000 miles in diameter before January 1, 2016?
(o 684 (goes-to no 80 #:start "2014-05-01" #:stop "2015-12-01")
	 (varying-strength "2014-05-01" "2015-12-01"))

;stable transactinide
(o 678 '(1 1 1 1 1
		 1 1 1 1 1
		 1 1 1 0.01 0.1
		 0.01 0.01 0.01 0.01 0.01
		 0.01 0.01 0.01 0.001 26))

; Will Virgin Galactic begin commercial flights aboard SpaceShipTwo by the end of 2014?
(o 670 (no 60) (varying-strength "2014-06-01" "2014-11-01"))

; Will a wearable camera with speed of more than 60 frames per second at 4K resolution be commercially available by the end of 2015?
(o 400 (yes 60) (varying-strength "2014-06-01" "2015-10-01"))
