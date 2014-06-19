#lang racket

(require math
         plot
         (planet bzlib/date/plt)
         (file "/home/jkominek/forecasting/questions.rkt")
         (file "/home/jkominek/forecasting/rankings.rkt")
         (file "/home/jkominek/forecasting/opinions.rkt")
         (file "/home/jkominek/forecasting/utils.rkt")
         (file "/home/jkominek/forecasting/trading.rkt")
         srfi/54
         )

(define question-ids (make-parameter #f))
(define question-database-path (make-parameter "tiny.json"))
(define my-user-name (make-parameter "jkominek"))
(define forced-debt-limit (make-parameter #f))
(define flip-optimization (make-parameter #f))

(define strategy (make-parameter utility-function))
(define strategy> (make-parameter comparison-function))

(void
 (command-line
  #:program "find-trades"
  #:once-each
  [("-d" "--db") db-path
                 "Question database to load"
                 (question-database-path db-path)]
  [("-u" "--user") user
                   "Username to trade for"
                   (my-user-name user)]
  [("--debt") debt-limit
              "Override debt limit; sign is flipped"
              (forced-debt-limit (- (string->number debt-limit)))]
  [("--minimize" "--flip") "Flip the optimization"
                           (flip-optimization #t)]

  #:once-any
  [("--curr-score") "Maximize current score"
                    (strategy (simple-adjustable 'curr-score))
                    (strategy> >)]
  [("--final-score") "Maximize final score"
                     (strategy (simple-adjustable 'final-score))
                     (strategy> >)]
  [("--credit") "Maximize transaction credit"
                (strategy (simple-adjustable 'credit))
                (strategy> >)]
  [("--debt") "Minimize final debt"
              (strategy (simple-adjustable '-debt))
              (strategy> >)]
  
  #:args raw-question-ids
  (question-ids (map string->number raw-question-ids))
  ))

(question-database (load-question-database (question-database-path)))

(when (null? (question-ids))
  (printf "need specific question ids for now~n")
  (exit)
  )

(when (flip-optimization)
  (strategy> (negate (strategy>))))

(for ([q-id (question-ids)])
  (when (have-opinion? q-id)
    (define q (fetch-full-question q-id))

    (define trade-sequence
      (find-optimal-trade-sequence
       (strategy) (strategy>)
       #:assets (question-user-assets q (my-user-name))
       #:debt-limit (if (number? (forced-debt-limit))
                        (forced-debt-limit)
                        (maximum-points-tied-up (question-settlement-at q)))
       #:beliefs (opinion-beliefs (get-opinion (question-id q)))
       #:initial-probabilities (question-probability q)
       #:trade-limit 5
       ))
    
    (if (empty? trade-sequence)
        (printf "nothing to do on ~a~n" q-id)
    
        (display
         (summarize-effect-of-trades
          q trade-sequence
          #:user-name (my-user-name)
          #:beliefs (opinion-beliefs (get-opinion (question-id q))))))
    ))