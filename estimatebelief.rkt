#lang racket

(require plot
         (planet bzlib/date/plt)
         (file "/home/jkominek/forecasting/questions.rkt"))

(question-database
  (load-question-database-url/cache-to-file
   *standard-question-list-url*
   "/home/jkominek/forecasting/data.json"
   #:max-age 10800))

(define q (fetch-full-question 727 #:max-age 10000))
(define raw-trades
  (sort (question-trades q)
        date>?
        #:key trade-created-at))

(define trades (map trade-assets raw-trades))

(define (loglikelihood trades)
  (define (helper belief)
    (for/sum ([trade trades]
              [i (in-naturals 1)])
      (- (log (- 1.0
                 (min 0.0
                      (/ (apply + (map *
                                       trade
                                       (list (- 1.0 belief)
                                             belief)))
                         i)))))))

  (define maximum
    (for/fold ([m -inf.0])
      ([p (in-range 0 101)])
      (max m (helper (/ p 100.0)))))

  (lambda (b)
    (- (helper b) maximum)))

(line-width 2)
(plot
 (list (function
        #:color 0
        (compose identity
                 (loglikelihood
                  (map trade-assets
                       (filter (lambda (t) (= (user-id (trade-user t)) 296))
                               raw-trades)))
                 ) 0 1)
       (function
        #:color 1
        (compose identity
                 (loglikelihood
                  (map trade-assets
                       (filter (lambda (t) (< (user-id (trade-user t)) 1500))
                               raw-trades)))
                 ) 0 1)
       (function
        #:color 2
        (compose identity
                 (loglikelihood
                  (map trade-assets
                       (filter (lambda (t) (>= (user-id (trade-user t)) 1500))
                               raw-trades)))
                 ) 0 1)
       ))