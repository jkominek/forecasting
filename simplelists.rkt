#lang racket

(require (file "/home/jkominek/forecasting/questions.rkt")
         (file "/home/jkominek/forecasting/utils.rkt")
	 (planet bzlib/date/plt)
         math/flonum)

(question-database
 (load-question-database-url/cache-to-file  *standard-question-list-url* "data.json"))

(define viable-questions
  (filter (lambda (q)
	    (and (question-visible? q)
		 (not (question-locked? q))))
	  (map fetch-question
	       (all-question-ids))))

(printf "************ NEW QUESTIONS~n")

(define (low-trade-question? q)
  (< (question-trade-count q) 6))

(define low-trade-questions
  (sort
   (filter low-trade-question?
	   viable-questions)
   date<?
   #:key question-created-at))

(for ([q viable-questions])
  (when (low-trade-question? q)
    (printf "(~a) id ~a \"~a\"~n"
	    (question-trade-count q)
	    (question-id q)
	    (question-name q))))

(printf "~n************ UNSWUNG QUESTIONS~n")

(define meaningful-volume-questions
  (filter (negate low-trade-question?) viable-questions))

(for ([q meaningful-volume-questions])
  (define mean (/ 1.0 (length (question-probability q))))
  (define stddev
    (sqrt (flsum (map (lambda (x) (expt (- x mean) 2))
                      (question-probability q)))))
  (when (< stddev 0.07)
    (printf "(~a) id ~a \"~a\"~n    ~a ** ~a~n"
	    (question-trade-count q)
	    (question-id q)
	    (question-name q)
	    stddev
            (pretty-probability-list (question-probability q)))))

(printf "~n************ UNSWUNG QUESTIONS~n")

(for ([q meaningful-volume-questions])
  (define highest (apply max (question-probability q)))
  (when (> highest 0.9)
    (printf "(~a) id ~a \"~a\"~n    ~a ** ~a~n"
	    (question-trade-count q)
	    (question-id q)
	    (question-name q)
	    highest
            (pretty-probability-list (question-probability q)))))

