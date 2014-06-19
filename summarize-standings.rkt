#lang racket

(require (file "/home/jkominek/forecasting/questions.rkt")
         (file "/home/jkominek/forecasting/rankings.rkt")
         (file "/home/jkominek/forecasting/opinions.rkt")
         (file "/home/jkominek/forecasting/utils.rkt")
         (planet bzlib/date/plt)
         srfi/54)

(question-database (load-question-database "tiny.json"))

(define q-id
  (command-line
   #:program "summarize-assets"
   #:args (q-id)
   (string->number q-id)))

(define question (fetch-full-question q-id))

(define N (length (question-probability question)))
(define (empty-assets)
  (make-vector N 0.0))
(define assets (make-hash))
(define market-assets (empty-assets))

(define trades
  (sort (question-trades question)
        date<?
        #:key trade-created-at))

(for ([trade trades])
  (define user-assets
    (hash-ref! assets (user-name (trade-user trade))
               empty-assets))
  (for ([asset (trade-assets trade)]
        [i (in-naturals)])
    (vector-set! user-assets i
                 (+ asset (vector-ref user-assets i)))
    (vector-set! market-assets i
                 (+ asset (vector-ref market-assets i)))
    ))

(define (potential-swing user-name)
  (for/fold ([swing 0])
    ([asset (hash-ref assets user-name)])
    (max swing (abs asset))))

(define users
  (sort (hash-keys assets) < #:key potential-swing))

(printf "~a~n" (question-name question))

(define (render-line user-name assets)
  (define expected-earnings
    (for/fold ([sum 0.0])
      ([probability (question-probability question)]
       [asset assets])
      (+ sum (* probability asset))))
  (printf "~a ~a  [~a ]~n"
          (cat user-name -15)
          (cat (exact-round expected-earnings) 5)
          (string-join
           (for/list ([asset assets])
             (cat (exact-round asset) 5))
           " ")))

(for ([user-name users])
  (define user-assets (hash-ref assets user-name))
  (render-line user-name user-assets))

(printf "---------------~n")
(render-line "    !MARKET!" (vector-map - market-assets))
(render-line " probability" (map (lambda (x) (* 100 x)) (question-probability question)))
