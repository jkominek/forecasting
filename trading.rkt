#lang racket/base

(require math
         math/flonum
         racket/contract
         racket/sequence
         racket/string
         (only-in racket/list last)
         (planet bzlib/date/plt)
         (file "/home/jkominek/forecasting/questions.rkt")
         (file "/home/jkominek/forecasting/rankings.rkt")
         (file "/home/jkominek/forecasting/opinions.rkt")
         (file "/home/jkominek/forecasting/utils.rkt")
         )

(define (cost->weight cost)
  (cond
    [(fl< cost 0.0) (fl+ 1.0 (flabs cost))]
    [(fl< cost 1.0) 1.0]
    [else 1.0]))

; We can't just through a full strength functionminimizer at
; this because we're very specifically constrained by the user
; interface of the web site. (for now?) We can only choose the
; new value of one choice at a time (all others are determined
; from there based on their past value) and it has to be
; (in-range 1/100 1 1/100)
(define
  (maximize-probability-adjustment
   #:probabilities initial-probabilities
   #:function f
   #:search-step [search-step 1]
   #:comparison [> >])
   
  ; these are all the choices which we'll consider adjusting.
  ; in the binary case, we only need to do one. otherwise we
  ; need to try adjusting all of them
  (define indexes
    (if (= (length initial-probabilities) 2)
        '(1)
        (sequence->list (in-range 0 (length initial-probabilities)))))

  ; loop over every choice, and all the possible new probabilities
  ; for it. compute the function we're maximizing, and check to see
  ; if the new set of probabilities should replace the old
  (for*/fold ([new-probabilities initial-probabilities]
              [maximized-value (f initial-probabilities #:initial #t)])
    ([choice indexes]
     [new-probability (in-range 1/100 1 (/ search-step 100))])
    (let* ([shifted-probabilities
            (shift-choice-probability
             initial-probabilities
             (list choice)
             (list (exact->inexact new-probability)))]
           [v (f shifted-probabilities)])
      ;(printf "~a ~a~n" shifted-probabilities v)
      ; we'll let f return void so it can say "screw those options, i don't even want to vote"
      (if (and (not (void? v))
               (> v maximized-value))
          (values shifted-probabilities v)
          (values new-probabilities maximized-value)))
    ))

(define (calc-per-asset-bankroll assets debt-limit)
  (for/list ([a assets])
    (if (>= a debt-limit)
        (max 1 (- a debt-limit))
        1)))

(define (kelly-utility #:beliefs beliefs
                       #:assets initial-assets
                       #:initial-probabilities initial-probabilities
                       #:debt-limit [debt-limit 0])
  (define bankroll (- debt-limit))
  (define per-asset-bankroll
    (calc-per-asset-bankroll initial-assets debt-limit))
  (define initial-debt (apply min initial-assets))

  (lambda (new-probabilities #:initial [initial #f])
    (define outcomes (lmsr-outcomes initial-probabilities new-probabilities))
    (define new-debt (apply min (map + outcomes initial-assets)))
    (let/ec no-good
      (when (and (< initial-debt debt-limit)
                 (< new-debt initial-debt))
            (no-good (void)))

      (for/sum ([b beliefs]
                [o outcomes]
                [pab per-asset-bankroll])
               (if (< (+ o pab) 0)
                   (no-good (void))
                   (* b (log (+ o pab))))))))

(define (utility-function
         #:beliefs beliefs
         #:assets initial-assets
         #:initial-probabilities initial-probabilities
         #:debt-limit [debt-limit 0])
  ; this is how much the market is currently holding onto
  ; until question resolution, initially
  (define initial-debt (apply min initial-assets))
  ; this is how much we expect to earn (probabilistically)
  ; upon question resolution, initially
  (unless (= (length beliefs) (length initial-assets))
    (printf "beliefs ~a mismatch assets ~a!~n" beliefs initial-assets))
  (define initial-expected-earnings (flsum (map fl* beliefs initial-assets))) 

  (lambda (new-probabilities #:initial [initial #f])
    (let* ([asset-change (lmsr-outcomes initial-probabilities new-probabilities)]
           [new-assets (map fl+ initial-assets asset-change)]
           ; this is the amount the market will hold onto until
           ; resolution if we make this trade
           [new-debt (apply min new-assets)]
           ; this is how much the market will refund us if we
           ; make this trade
           [credit (- new-debt initial-debt)]
           ; this is how much we expect to earn (probabilistically)
           ; upon question resolution, if we make this trade
           [new-expected-earnings
            (flsum (map * beliefs new-assets))]
           ; change in expected earnings
           [expected-earnings-improvement
            (- new-expected-earnings initial-expected-earnings)])
      
      ; check that we're actually improving things in the end
      (if (or initial
              (and (> expected-earnings-improvement 0)
		   ; you can exceed the debt limit by x if you're
		   ; going to get e^x improvement in expected earnings
		   (or (> new-debt initial-debt)
		       (>= new-debt (- debt-limit
				       (if (> expected-earnings-improvement 1.0)
					   (log expected-earnings-improvement)
					   0.0))))))
          ; if so
	  (if (>= new-debt (/ debt-limit 10))
	      ; anything under 10% of our debt limit
	      ; isn't worth penalizing the earnings for
	      (list 1.0
		    (* new-expected-earnings
		       (+ 1.0 (log (if (< new-debt 1.0)
                                       1.0
                                       new-debt)))))
	      ; but over that, we need to scale them
	      (list 0.0
		    (/ new-expected-earnings
		       (abs new-debt))))
          
          ; this is the "we're making things worse" case. fuck it
          (void)))))

(define (python-utility
         #:beliefs beliefs
         #:assets initial-assets
         #:initial-probabilities initial-probabilities
         #:debt-limit [debt-limit 0])
  ; this is how much the market is currently holding onto
  ; until question resolution, initially
  (define initial-debt (apply min initial-assets))
  ; this is how much we expect to earn (probabilistically)
  ; upon question resolution, initially
  (define initial-expected-earnings (flsum (map * beliefs initial-assets))) 

  (lambda (new-probabilities #:initial [initial #f])
    (let* ([asset-change (lmsr-outcomes initial-probabilities new-probabilities)]
           [new-assets (map + initial-assets asset-change)]
           ; this is the amount the market will hold onto until
           ; resolution if we make this trade
           [new-debt (apply min new-assets)]
           ; this is how much we expect to earn (probabilistically)
           ; upon question resolution, if we make this trade
           [new-expected-earnings
            (flsum (map * beliefs new-assets))]
           ; change in expected earnings
           [expected-earnings-improvement
            (- new-expected-earnings initial-expected-earnings)])
      
      ; check that we're actually improving things in the end
      (if (or initial
              (and (> expected-earnings-improvement 0)
		   ; you can exceed the debt limit by x if you're
		   ; going to get e^x improvement in expected earnings
		   ; this part wasn't in the python version, but seems
		   ; safe enough
                   (>= new-debt (- debt-limit
				   (if (> expected-earnings-improvement 1.0)
				       (log expected-earnings-improvement)
				       0.0)))))
	  (list (if (for/fold ([some-improvement #f])
			      ([new new-assets]
			       [old initial-assets])
			      (or some-improvement (> new old)))
		    1
		    0)
		(/ new-expected-earnings (cost->weight (apply min new-assets)))
		(apply min new-assets))
          ; this is the "we're making things worse" case. fuck it
          (void)))))

(define (simple-adjustable
         attribute)
  (lambda (#:beliefs beliefs
           #:assets initial-assets
           #:initial-probabilities initial-probabilities
           #:debt-limit [debt-limit 0])
    ; this is how much the market is currently holding onto
    ; until question resolution, initially
    (define initial-debt (apply min initial-assets))
    ; this is how much we expect to earn (probabilistically)
    ; upon question resolution, initially
    (define initial-expected-earnings (flsum (map * beliefs initial-assets))) 
    
    (lambda (new-probabilities #:initial [initial #f])
      (let* ([asset-change (lmsr-outcomes initial-probabilities new-probabilities)]
             [new-assets (map + initial-assets asset-change)]
             ; this is the amount the market will hold onto until
             ; resolution if we make this trade
             [new-debt (apply min new-assets)]
             ; this is how much the market will refund us if we
             ; make this trade
             [credit (- new-debt initial-debt)]
             ; this is how much we expect to earn (probabilistically)
             ; upon question resolution, if we make this trade
             [new-expected-earnings
              (flsum (map * beliefs new-assets))]
             ; change in expected earnings
	     [expected-earnings-improvement
              (- new-expected-earnings initial-expected-earnings)])
        
        (if (or (> new-debt debt-limit)
		(> new-debt initial-debt)
		initial)
            (cond
              [(equal? attribute 'curr-score)
               (- (flsum (map * new-assets new-probabilities))
                  (flsum (map * initial-assets initial-probabilities)))]
              [(equal? attribute 'final-score)
               expected-earnings-improvement]
              [(equal? attribute 'final+assetinc)
               (if (> (flsum asset-change) 0)
                   expected-earnings-improvement
                   -1000)]
              [(equal? attribute 'credit)
               credit]
              [(equal? attribute '-debt)
               (- new-debt)]
              [(equal? attribute 'final/-debt)
               (/ new-expected-earnings
                  (if (< new-debt 0.0)
                      (+ 1.0 (abs new-debt))
                      1.0))]
              [(equal? attribute 'total-positive-assets)
               (for/fold ([sum 0.0]) ([asset new-assets])
                         (+ sum (if (> asset 0.0) asset 0)))]
              [(equal? attribute 'total-assets)
               (flsum new-assets)]
              [(equal? attribute 'assets/debt)
               (/ (for/fold ([sum 0.0]) ([asset new-assets])
                            (+ sum (if (> asset 0.0) asset 0)))
                  (cost->weight (apply min new-assets)))]
              [else (error "unknown attribute" attribute)])
            (void)
            )))))

(define (comparison-function as bs)
  (let/ec done
    (for ([a as]
	  [b bs])
      (cond
       [(fl> a b) (done #t)]
       [(fl< (flabs (fl- a b)) 1e-10) (void)]
       [(fl< a b) (done #f)]))
    #f))

(define (void-safe-comparison >)
  (lambda (a b)
    (cond
     [(and (void? a) (void? b)) #f]
     [(and (not (void? a)) (void? b)) #t]
     [(and (void? a) (not (void? b))) #f]
     [else (> a b)])))

(define (search-helper current
                       best
                       uf comparison
                       #:current-utility [cu (uf current)]
                       #:best-utility [bu (uf best)]
                       #:iterations [remaining 150]
                       #:temperature [temp 0.2])
  (define unnormalized
    (for/list ([p (in-list current)])
      (sample (truncated-dist (normal-dist p temp) 0.001 0.999))))
  (define normalized
    (normalize-probabilities unnormalized))

  (define newu (uf normalized))

  (if (comparison newu cu)
      (let ([new-best? (comparison newu bu)])
        (if (<= remaining 0)
            (if new-best? normalized best)
            (search-helper normalized
                           (if new-best? normalized best)
                           uf comparison
                           #:current-utility newu
                           #:best-utility (if new-best? newu bu)
                           #:iterations (sub1 remaining)
                           #:temperature (* temp 0.98))))
      (if (<= remaining 0)
          best
          (search-helper current best
                         uf comparison
                         #:current-utility cu
                         #:best-utility bu
                         #:iterations (sub1 remaining)
                         #:temperature (* temp 0.98)))))

(define (search-optimal-trade
         utility-function
         comparison-function
         #:assets assets
         #:debt-limit debt-limit
         #:initial-probabilities initial-probabilities
         #:beliefs beliefs
         #:minimum-change [ignore1 0]
         #:trade-limit [ignore2 0])
  (define uf (utility-function 
              #:beliefs beliefs
              #:assets assets
              #:initial-probabilities initial-probabilities
              #:debt-limit debt-limit))
  (list
   (search-helper initial-probabilities initial-probabilities uf
                  (void-safe-comparison comparison-function))))

(define (find-optimal-trade
         utility-function
         comparison-function
         #:search-step [search-step 1]
         #:assets assets
         #:debt-limit debt-limit
         #:initial-probabilities initial-probabilities
         #:beliefs beliefs)
  (define-values
    (new-probabilities utility)
    (maximize-probability-adjustment
     #:probabilities initial-probabilities
     #:search-step search-step
     #:function (utility-function
                 #:beliefs beliefs
                 #:assets assets
                 #:initial-probabilities initial-probabilities
                 #:debt-limit debt-limit)
     #:comparison comparison-function))
  new-probabilities)

(define (find-optimal-trade-sequence
         utility-function
	 comparison-function
         #:assets initial-assets
         #:debt-limit debt-limit
         #:beliefs beliefs
         #:search-step [search-step 1]
	 #:minimum-change [minimum-change 1/3]
         #:initial-probabilities initial-probabilities
         #:trade-limit [trade-limit 10])
  (let/ec done
    (when (= trade-limit 0)
      (done '()))
    (define next-trade
      (find-optimal-trade utility-function comparison-function
                          #:assets initial-assets #:beliefs beliefs
                          #:initial-probabilities initial-probabilities
                          #:debt-limit debt-limit))
    ;(printf "~a~n" next-trade)
    (define max-difference
      (for/fold ([diff 0])
		([p initial-probabilities]
		 [n next-trade])
		(max diff (abs (- p n)))))
    (when (< max-difference (/ minimum-change 100))
      (done '()))
    (define new-assets (map + initial-assets (lmsr-outcomes initial-probabilities next-trade)))
    (cons next-trade
          (find-optimal-trade-sequence
           utility-function comparison-function
           #:assets new-assets #:beliefs beliefs
           #:initial-probabilities next-trade
           #:debt-limit debt-limit
           ; search with smaller resolution on the cleanup
           #:search-step 1/3
           ; we've already found one trade that is worth executing
           ; further improvements are "free" these days
           #:minimum-change 1/20
           #:trade-limit (sub1 trade-limit)))))

(define (determine-choice ps)
  ; better hope that one of the choices matches
  ; or this will explode. hooray
  (let ([p (car ps)])
    (if (or (not (<= 0.0 p 1.0))
	    (< (abs (- (/ (round (* p 100)) 100) p)) 1e-14))
	0
	(if (null? (cdr ps))
            0 ;(error "couldn't find the choice")
	    (add1 (determine-choice (cdr ps)))))))

(define/contract
  (summarize-effect-of-trades
   question new-probabilities-list
   #:beliefs [beliefs #f]
   #:initial-assets [initial-assets #f]
   #:debt-limit [debt-limit (maximum-points-tied-up (question-settlement-at question))]
   #:summary-hash [sh (make-hash)]
   #:user-name my-user-name)
  (->* ((hash/c symbol? any/c)
	(non-empty-listof (non-empty-listof (real-in 0.0 1.0)))
	#:user-name string?)
       (#:debt-limit real?
	#:initial-assets (or/c #f (non-empty-listof real?))
	#:beliefs (or/c #f (non-empty-listof (real-in 0.0 1.0)))
	#:summary-hash (hash/c symbol? number?))
       string?)

  (define initial-probabilities (question-probability question))
  (unless initial-assets
    (set! initial-assets (question-user-assets question my-user-name)))
  (define Δassets
    (for/list ([from (cons initial-probabilities new-probabilities-list)]
               [to new-probabilities-list])
       (lmsr-outcomes from to)))
  (define-values (reversed-assets-per-trade ignore)
    (for/fold ([assets '()]
               [initial initial-assets])
      ([Δ Δassets])
      (let ([new (map + Δ initial)])
        (values (cons new assets)
                new))))
  (define assets-per-trade (reverse reversed-assets-per-trade))
  (define credit (- (apply min (last assets-per-trade)) (apply min initial-assets)))

  ;(hash-set! sh 'initial-probabilities initial-probabilities)
  ;(hash-set! sh 'initial-assets initial-assets)
  (hash-set! sh 'credit credit)
  (hash-set! sh 'total-Δassets  (flsum (map - (last assets-per-trade) initial-assets)))

  (define initial-current-score (flsum (map * initial-assets initial-probabilities)))
  (define new-current-score (flsum (map * (last assets-per-trade) (last new-probabilities-list))))

  (hash-set! sh 'initial-current-score initial-current-score)
  (hash-set! sh 'new-current-score new-current-score)
  (hash-set! sh 'current-score-improvement (- new-current-score initial-current-score))
  
  (string-join
   (list
    (cat "debt limit:" 15)
    (cat debt-limit)
    "\n"
    (cat "belief:" 15)
    (if beliefs
        (pretty-probability-list beliefs)
        " ? ")
    "\n"
    (cat "cur prob:" 15)
    (pretty-probability-list initial-probabilities)
    (cat "cred" 6)
    (cat "curr" 6)
    (if beliefs
        (cat "fin" 6)
        "")
    "\n"
    (for/fold ([s ""])
      ([from (cons initial-probabilities new-probabilities-list)]
       [to new-probabilities-list]
       [old-assets (cons initial-assets assets-per-trade)]
       [new-assets assets-per-trade]
       [id (in-range 1 50)])
      (define choice (determine-choice to))
      (string-join
       (list s
             (cat "" 15)
             (pretty-string-list (for/list ([i (in-range 0 (length to))])
                                   (if (= i choice)
                                       "  ↓  " "     ")))
	     "  "
	     (cat (if (>= choice 0)
		      (string-trim #:repeat? #t (choice-name (question-choice question choice)))
		      "???") -16)
	     
             "\n"
             (cat id 14) " "
             (pretty-probability-list to)
             " "
             (cat (- (apply min new-assets) (apply min old-assets)) 5 -1.)
             " "
             (cat (- (flsum (map * to new-assets)) (flsum (map * from old-assets))) 5 -1.)
             " "
             (if beliefs
                 (cat (- (flsum (map * beliefs new-assets)) (flsum (map * beliefs old-assets))) 5 -1.)
                 "")
             "\n")
       ""))
    (cat "orig assets:" 15)
    (pretty-asset-list initial-assets)
    "\n"
    (cat "Δ assets:" 15)
    (pretty-asset-list (map - (last assets-per-trade) initial-assets))
    (cat (round (flsum (map - (last assets-per-trade) initial-assets))) 6)
    "\n"
    (cat "final assets:" 15)
    (pretty-asset-list (last assets-per-trade))
    "\n"
    (if (> credit 0)
        (string-join (list (cat "credit:" 15) (cat credit 8 -2.)) "")
        (string-join (list (cat "debit:" 15) (cat (- credit) 8 -2.)) ""))
    "\n"
    (cat "curr score:" 15)
    (cat (- new-current-score initial-current-score) 8 -2.)
    "\n"
    (if beliefs
        (let* ([initial-final-score (flsum (map * initial-assets beliefs))]
               [new-final-score (flsum (map * (last assets-per-trade) beliefs))])
	  (hash-set! sh 'initial-final-score initial-final-score)
	  (hash-set! sh 'new-final-score new-final-score)
	  (hash-set! sh 'final-score-improvement (- new-final-score initial-final-score))
          (hash-set! sh 'initial-kelly (simple-kelly beliefs initial-assets debt-limit))
          (hash-set! sh 'final-kelly (simple-kelly beliefs (last assets-per-trade) debt-limit))
          (hash-set! sh 'kelly-improvement
                     (- (hash-ref sh 'final-kelly) (hash-ref sh 'initial-kelly)))

          (string-join
           (list (cat "final score:" 15)
                 (cat (- new-final-score initial-final-score) 8 -2.)
                 "\n"
		 (cat "exp earnings:" 15)
		 (cat new-final-score 8 -2.)
		 "\n"
                 (cat "kelly util:" 15)
                 (cat (hash-ref sh 'final-kelly) 8 -2.)
                 "  change: "
                 (cat (hash-ref sh 'kelly-improvement) 8 -4.)
                 "\n"
           ) ""))
        "")
    ) ""))

(define (simple-kelly beliefs assets debt-limit)
  (let ([pab (calc-per-asset-bankroll assets debt-limit)])
    (sum (map *
              beliefs
              (map log pab)))))

(provide utility-function
         kelly-utility
	 python-utility
         simple-adjustable
         comparison-function
 
         search-optimal-trade
         find-optimal-trade
         find-optimal-trade-sequence
         
         summarize-effect-of-trades
         )
