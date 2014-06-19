#lang racket/base

(require (planet bzlib/date/plt)
	 json
	 racket/set
	 racket/contract
	 )

(define question-database/c (hash/c natural-number/c jsexpr?))
(define question/c jsexpr?)

(define/contract
  (load-question-database path)
  (-> path-string? question-database/c)

  (let ([raw-json (read-json (open-input-file path))])
    (for/hash ([q-rec (in-list raw-json)])
      (values (hash-ref (hash-ref q-rec 'question) 'id)
	      q-rec))))

(define/contract
  question-database
  (parameter/c (hash/c natural-number/c jsexpr?))

  (make-parameter #f))

(provide question-database load-question-database)

(define/contract
  (fetch-question id #:question-database [q-d (question-database)])
  (->* (natural-number/c) (#:question-database question-database/c) question/c)

  (hash-ref q-d id))

(define/contract
  (fetch-full-question q-or-qid #:question-database [q-d (question-database)])
  (->* ((or/c natural-number/c question/c))
       (#:question-database question-database/c)
       question/c)

  (let* ([qid (if (number? q-or-qid)
		  q-or-qid
		  (question-id q-or-qid))]
	 [full
	  (read-json
	   (open-input-file
	    (build-path "q" (number->string qid))))])
    (for/fold ([start (fetch-question qid)])
	      ([(k v) (in-hash full)])
      (hash-set start k v))))

(define/contract (all-question-ids
		  #:question-database [q-d (question-database)])
  (->* () (#:question-database question-database/c) (listof natural-number/c))

  (hash-keys q-d))

(provide fetch-question fetch-full-question all-question-ids)

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

(define (question-settlement-at q)
  (read-iso8601 (hash-ref (hash-ref q 'question) 'settlement_at)))

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

(define/contract (question-choice q i)
  (-> jsexpr? natural-number/c (hash/c symbol? any/c))

  (list-ref (question-choices q) i))

(define/contract (question-trades q)
  (-> jsexpr? (listof jsexpr?))
  
  (hash-ref q 'trades))

(provide question-categories question-name question-short-name question-id
	 question-probability question-keywords question-updated-at
	 question-settlement-at question-created-at question-probability-at
	 question-challenge question-description question-visible?
	 question-locked? question-choices question-choice
	 question-trade-count question-comment-count question-trades)

(define (choice-name c)
  (hash-ref c 'name))

(define (choice-locked? c)
  (hash-ref c 'is_locked))

(provide choice-name choice-locked?)

(define/contract (trade-user t)
  (-> jsexpr? jsexpr?)
  
  (hash-ref t 'user))

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

(provide trade-user trade-created-at trade-old-values trade-new-values trade-assets)

(define/contract (user-name u)
  (-> jsexpr? string?)
  
  (hash-ref u 'username))

(define/contract (user-id u)
  (-> jsexpr? natural-number/c)
  
  (hash-ref u 'id))

(provide user-name user-id)

