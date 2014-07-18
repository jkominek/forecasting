#lang racket

(require (file "/home/jkominek/forecasting/questions.rkt")
         ;(file "/home/jkominek/forecasting/rankings.rkt")
         (file "/home/jkominek/forecasting/opinions.rkt")
         (file "/home/jkominek/forecasting/utils.rkt")
         (file "/home/jkominek/forecasting/trading.rkt")
         (planet bzlib/date/plt)
         )

(question-database
  (load-question-database-url/cache-to-file *standard-question-list-url* "data.json"
                                            #:max-age 86400))

(define u-func
  (simple-adjustable 'assets/debt))

(define standings (make-hash))
(for ([trade (fetch-user-trades 296 #:max-age 86400)])
  (define q-id (hash-ref trade 'question_id))
  (update-standings q-id (trade-assets trade)
                    #:standings standings))

(define (analyze q)
  (define choices (length (question-probability q)))
  (define remaining (- (date->seconds (question-settlement-at q))
                       (current-seconds)))
  (define remaining-days (/ remaining 86400))
  (define assets
    (hash-ref standings (question-id q)
              (thunk
               (for/list ([x (in-range 0 choices)]) 0.0))))
  (define fake-beliefs (normalize-probabilities
                        (for/list ([x (in-range 0 choices)]) 1)))
  (define debt-limit (- (/ (log remaining-days) 1)))
  (define trade-seq
    (find-optimal-trade-sequence
     u-func
     >
     #:assets assets
     #:debt-limit debt-limit
     #:minimum-change 1/10
     #:beliefs fake-beliefs
     #:initial-probabilities (question-probability q)))
  ;(printf "~a~n" trade)
  (when (> (length trade-seq) 0)
    (printf "~a~n" (summarize-effect-of-trades
     q trade-seq
     #:beliefs fake-beliefs
     #:initial-assets assets
     #:debt-limit debt-limit
     #:user-name ""))
    ))

(for ([q-id (all-question-ids)]
;      [x (in-range 0 10)]
      #:unless (have-opinion? q-id))
  (define q (fetch-question q-id))
  (when
   (and (question-visible? q)
        (not (question-locked? q))
        (or (not (hash-has-key? q 'settled_at))
            (null? (hash-ref q 'settled_at))))
   (define remaining (- (date->seconds (question-settlement-at q))
                        (current-seconds)))
   (define remaining-days (/ remaining 86400))
   (when (and (> remaining-days 180)
              (> (question-trade-count q) 25))
     (printf "~a ~a~n" q-id (exact-round remaining-days))
     (analyze q)
     )))
