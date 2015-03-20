#lang racket/base

(require (planet bzlib/date/plt)
	 json
	 racket/set
	 racket/contract
	 (file "/home/jkominek/forecasting/utils.rkt")
	 )

(define question-database/c (hash/c natural-number/c jsexpr?))
(define question/c jsexpr?)

(define/contract
  (load-question-database port)
  (-> input-port? question-database/c)

  (let ([raw-json (read-json port)])
    (for/hash ([q-rec (in-list raw-json)])
      (values (hash-ref (hash-ref q-rec 'question) 'id)
	      q-rec))))

(define/contract
  (load-question-database-url/cache-to-file url path #:max-age [max-age 1800])
  (->* (string? path-string?) (#:max-age natural-number/c) question-database/c)
  (load-question-database
    (open-url/cache-to-file url path #:max-age max-age)))

(define/contract
  question-database
  (parameter/c (hash/c natural-number/c jsexpr?))

  (make-parameter #f))

(define my-user-name (make-parameter #f))
(define my-user-id (make-parameter #f))

(define *standard-question-list-url* (format "https://scicast.org/questions/index?include_prob=True&include_comment_count=True&include_trade_count=True&include_user_roles=False&include_question_clique=False&include_question_relationship=False&api_key=~a" (api-key)))

(provide question-database load-question-database
	 load-question-database-url/cache-to-file *standard-question-list-url*
	 )

(define/contract
  (have-question? id #:question-database [q-d (question-database)])
  (->* (natural-number/c) (#:question-database question-database/c) (or/c #t #f))

  (hash-has-key? q-d id))

(define/contract
  (fetch-question id #:question-database [q-d (question-database)])
  (->* (natural-number/c) (#:question-database question-database/c) question/c)

  (hash-ref q-d id))

(define/contract
  (update-question id newq #:question-database [q-d (question-database)])
  (->* (natural-number/c jsexpr?) (#:question-database question-database/c) void?)

  (question-database (hash-set (question-database) id newq))

  (void))

(define (question-url q-id)
  (format
   "http://scicast.org/questions/show?question_id=~a&include_prob=True&include_cash=True&include_trades=True&include_comments=False&include_trade_ranges=True&include_recommendations=False&api_key=~a"
   q-id (api-key)))

(define/contract
  (fetch-full-question q-or-qid
		       #:max-age [max-age 3600]
		       #:question-database [q-d (question-database)])
  (->* ((or/c natural-number/c question/c))
       (#:max-age natural-number/c
	#:question-database question-database/c)
       question/c)

  (let* ([qid (if (number? q-or-qid)
		  q-or-qid
		  (question-id q-or-qid))]
	 [question (fetch-question qid)]
	 [qpath (build-path "/home/jkominek/forecasting/q" (number->string qid))]
	 [current-date
	  (if (file-exists? qpath)
	      (question-updated-at
	       (read-json
		(open-input-file qpath)))
	      (read-iso8601 "1980-01-01"))])
    (define full
      (if (date<? current-date
		  (question-updated-at question))
	  ; oh noes full question is out of date
	  (read-json (open-url/cache-to-file (question-url qid) qpath #:max-age max-age))
	  ; up to date, use what we've got
	  (read-json (open-input-file qpath))))
    (for/fold ([start question])
	      ([(k v) (in-hash full)])
      (hash-set start k v))))

(define/contract (all-question-ids
		  #:question-database [q-d (question-database)])
  (->* () (#:question-database question-database/c) (listof natural-number/c))

  (hash-keys q-d))

(provide fetch-question update-question fetch-full-question all-question-ids have-question?)

(struct category [id name] #:transparent #:constructor-name make-category)

(provide category-id category-name)

(define (question-trade-count q)
  (if (hash-has-key? q 'trades)
      (length (hash-ref q 'trades))
      (hash-ref q 'trade_count)))

(define (question-comment-count q)
  (if (hash-has-key? q 'comments)
      (length (hash-ref q 'comments))
      (hash-ref q 'comment_count)))

(define (question-categories q)
  (map (lambda (js)
	 (make-category (hash-ref js 'id)
			(hash-ref js 'name)))
       (hash-ref q 'question_categories)))

(define (question-name q)
  (hash-ref (hash-ref q 'question) 'name))

(define (question-short-name q)
  (hash-ref (hash-ref q 'question) 'short_name))

(define (question-id q)
  (hash-ref (hash-ref q 'question) 'id))

(define (question-probability q)
  (hash-ref q 'prob))

(define (question-keywords q)
  (hash-ref (hash-ref q 'question) 'keywords))

(define (question-updated-at q)
  (read-iso8601 (hash-ref (hash-ref q 'question) 'updated_at)))

(define/contract
  (question-settlement-at q)
  (-> question/c (or/c date? #f))
  (let ([v (hash-ref (hash-ref q 'question) 'settlement_at)])
    (if (string? v)
	(read-iso8601 v)
	#f)))

(define (question-settled-at q)
  (let ([v (hash-ref (hash-ref q 'question) 'settled_at #f)])
    (if (string? v)
	(read-iso8601 v)
	#f)))

(define (question-created-at q)
  (read-iso8601 (hash-ref (hash-ref q 'question) 'created_at)))

(define (question-probability-at q)
  (read-iso8601 (hash-ref (hash-ref q 'question) 'probability_at)))

(define (question-challenge q)
  (hash-ref (hash-ref q 'question) 'challenge))

(define (question-description q)
  (hash-ref (hash-ref q 'question) 'desc))

(define (question-visible? q)
  (hash-ref (hash-ref q 'question) 'is_visible))

(define (question-ordered? q)
  (hash-ref (hash-ref q 'question) 'is_ordered))

(define (question-locked? q)
  (hash-ref (hash-ref q 'question) 'is_locked))

(define/contract (question-choices q)
  (-> jsexpr? (listof (hash/c symbol? any/c)))

  (if (equal? "binary" (hash-ref (hash-ref q 'question) 'type))
      (list (hasheq 'is_locked #f
		    'name "No")
	    (hasheq 'is_locked #f
		    'name "Yes"))
      (if (hash-has-key? q 'question_choices)
	  (hash-ref q 'question_choices)
	  (hash-ref (hash-ref q 'question) 'choices))))

(define/contract (question-choices-locked q)
  (-> jsexpr? (listof boolean?))
  (map (lambda (c) (hash-ref c 'is_locked)) (question-choices q)))

(define/contract (question-choice q i)
  (-> jsexpr? natural-number/c (hash/c symbol? any/c))

  (list-ref (question-choices q) i))

(define/contract (question-trades q)
  (-> jsexpr? (listof jsexpr?))
  
  (hash-ref q 'trades))

(define/contract (question-user-assets q user-name-or-id)
  (-> jsexpr? (or/c string? natural-number/c) (listof real?))
  (for/fold ([assets (map (lambda x 0.0) (question-probability q))])
    ([trade (question-trades q)])
    (let ([user (trade-user trade)])
      (if (equal? user-name-or-id
                  ((if (number? user-name-or-id)
                       user-id
                       user-name)
                   user))
          (map + assets (trade-assets trade))
          assets))))

(provide question-categories question-name question-short-name question-id
	 question-probability question-keywords question-updated-at
	 question-settlement-at question-settled-at question-created-at question-probability-at
	 question-challenge question-description question-visible?
	 question-locked? question-choices question-choice
	 question-trade-count question-comment-count question-trades
         question-user-assets question-ordered? question-choices-locked)

(define (choice-name c)
  (hash-ref c 'name))

(define (choice-locked? c)
  (hash-ref c 'is_locked))

(provide choice-name choice-locked?)

(define/contract (trade-user t)
  (-> jsexpr? jsexpr?)
  
  (hash-ref t 'user))

(define (trade-id t)
  (hash-ref t 'id))

(define (trade-question t)
  (hash-ref t 'question))

(define (trade-assumptions t)
  (if (hash-has-key? t 'assumptions)
      (hash-ref t 'assumptions)
      '()))

(define (trade-created-at t)
  (read-iso8601 (hash-ref t 'created_at)))

(define (trade-old-values t)
  (-> jsexpr? (listof (between/c 0.0 1.0)))
  
  (hash-ref t 'old_value_list))

(define (trade-new-values t)
  (-> jsexpr? (listof (between/c 0.0 1.0)))
  
  (hash-ref t 'new_value_list))

(define (trade-assets t)
  (-> jsexpr? (listof number?))
  
  (hash-ref t 'assets_per_option))

(provide trade-id trade-user trade-question trade-assumptions trade-created-at trade-old-values trade-new-values trade-assets)

(define/contract (user-name u)
  (-> jsexpr? string?)
  
  (hash-ref u 'username))

(define/contract (user-id u)
  (-> jsexpr? natural-number/c)
  
  (hash-ref u 'id))

(provide user-name user-id)

(define/contract
  trade-database
  (parameter/c (hash/c natural-number/c (listof any/c)))
  (make-parameter (make-hash)))

(define (user-trades-url user-id [newer-than #f])
  (if newer-than
      (set! newer-than (format "&newer_than=~a" newer-than))
      (set! newer-than ""))
  (format "https://scicast.org/trades/?user_id=~a&include_current_probs=False~a&api_key=~a"
          user-id newer-than (api-key)))

(define/contract
  (fetch-user-trades user-id
                     #:newer-than [newer-than #f]
                     #:max-age [max-age 1800])
  (->* (natural-number/c) (#:max-age natural-number/c) jsexpr?)

  (if (hash-has-key? (trade-database) user-id)
      (hash-ref (trade-database) user-id)
      (let* ([utpath (build-path "ut" (number->string user-id))]
	     [v (read-json (open-url/cache-to-file (user-trades-url user-id newer-than)
                                                   utpath
                                                   #:max-age max-age))])
	(hash-set! (trade-database) user-id v)
	v)))

(provide fetch-user-trades)
