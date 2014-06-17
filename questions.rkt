#lang racket/base

(require (planet bzlib/date/plt)
	 json
	 racket/set
	 racket/contract
	 )

(define question-database/c (hash/c natural-number/c jsexpr?))
(define question/c jsexpr?)

(define/contract
  (load-database path)
  (-> path-string? question-database/c)

  (let ([raw-json (read-json (open-input-file path))])
    (for/hash ([q-rec (in-list raw-json)])
      (values (hash-ref (hash-ref q-rec 'question) 'id)
	      q-rec))))

(define/contract
  question-database
  (parameter/c (hash/c natural-number/c jsexpr?))

  (make-parameter (load-database "data.json")))

(define/contract
  (fetch-question id #:question-database [q-d (question-database)])
  (->* (natural-number/c) (#:question-database question-database/c) question/c)

  (hash-ref q-d id))

(define/contract (all-question-ids
		  #:question-database [q-d (question-database)])
  (->* () (#:question-database question-database/c) (listof natural-number/c))

  (hash-keys q-d))

(provide fetch-question all-question-ids)

(struct category [id name] #:transparent #:constructor-name make-category)

(provide category-id category-name)

(define (question-trade-count q)
  (hash-ref q 'trade_count))

(define (question-comment-count q)
  (hash-ref q 'comment_count))

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

(provide question-categories question-name question-short-name question-id
	 question-probability question-keywords question-updated-at
	 question-settlement-at question-created-at question-probability-at
	 question-challenge question-description question-visible?
	 question-locked? question-choices question-choice
	 question-trade-count question-comment-count)


(define (choice-name c)
  (hash-ref c 'name))

(define (choice-locked? c)
  (hash-ref c 'is_locked))

(provide choice-name choice-locked?)
  
