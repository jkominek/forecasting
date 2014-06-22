#lang racket/base

(require (planet bzlib/date/plt)
	 db/base db/sqlite3
	 racket/contract
	 plot)

(define conn (sqlite3-connect #:database "/home/jkominek/forecasting/rankings.db"))

(define/contract
  (user-minimum-score user)
  (-> (or/c natural-number/c
	    (hash/c symbol? any/c))
      real?)

  (let* ([user-id (if (number? user)
		      user
		      (hash-ref user 'user_id))]
	 [score (query-maybe-value conn
		  "select min(score) from rankings where id=$1 and date('now', '-7 day')<=day"
		  user-id)])
    (if (number? score)
	score
	(error "user-minimum-score isn't yet smart enough to fetch new data"))))

;(define/contract
;  (todays-top-n n)
;  (-> natural-numer/c (non-empty-listof (non-empty-listof any/c)))

(provide user-minimum-score)

(define (retrieve-history [type 'rank])
  (define history (make-hash))
  (for ([(user-id day rank score)
	 (in-query conn
		   "select id, day, rank, score from rankings order by day"
		   #:fetch 64)])
    (when (number? rank)
      (hash-update! history user-id
		    (lambda (orig)
		      (cons (vector (date->seconds (read-iso8601 day))
				    (if (equal? type 'rank)
					rank
					score)) orig))
		    list)))

  (for/list ([l (in-hash-values history)])
    (reverse l)))

(define flip-transform 
  (invertible-function - -))

(define (plot-ranks)
  (parameterize (;[plot-y-ticks (ticks-scale (plot-y-ticks)
                 ;                           flip-transform)]
                 ;[plot-y-transform (make-axis-transform flip-transform)]
		 [plot-x-ticks (date-ticks)])
    (plot (map lines (retrieve-history 'rank))
	  )))

(define (plot-scores)
  (parameterize ([plot-x-ticks (date-ticks)]
		 [plot-y-transform (axis-transform-compose
				    (stretch-transform 4000 6000 3.0)
				    (collapse-transform 12500 13500))])
    (plot (map lines (retrieve-history 'score))
          #:y-min 0
	  )))

(provide plot-ranks plot-scores)
