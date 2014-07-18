#lang racket/base

(require math
         racket/cmdline
         racket/match
         (planet bzlib/date/plt)
         (file "/home/jkominek/forecasting/questions.rkt")
         (file "/home/jkominek/forecasting/opinions.rkt")
         (file "/home/jkominek/forecasting/utils.rkt")
         )

(question-database (load-question-database-url/cache-to-file *standard-question-list-url* "data.json" #:max-age 9000))

(void
  (command-line
   #:program "summarize-assets"
   ))

(for ([q-id (all-question-ids)]
      #:when (have-opinion? q-id))
  (when (not (= (length (question-probability (fetch-question q-id)))
		(length (opinion-beliefs (get-opinion q-id)))))
    (printf "opinion mismatch ~a~n" q-id)))

(define max-trades 0)

(for ([q-id (sort (all-question-ids) <)]
      #:unless (have-opinion? q-id))

  (define q (fetch-question q-id))

  (when (> (question-trade-count q) max-trades)
    (set! max-trades (question-trade-count q)))

  (when
   (and (question-visible? q)
        (not (question-locked? q))
        (or (not (hash-has-key? q 'settled_at))
            (null? (hash-ref q 'settled_at)))
        (> (question-trade-count q) 0 #;(/ max-trades 10)))

   (printf "#~a \"~a\"~n"
	   q-id (question-name q))
   (printf "  Trades: ~a  Comments: ~a~n"
	   (question-trade-count q)
	   (question-comment-count q))
   (printf "  Stands at: ~a~n"
	   (pretty-probability-list (question-probability q)))

   ))
