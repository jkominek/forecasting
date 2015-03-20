#lang racket/base

(require (planet bzlib/date/plt)
         racket/set
         racket/list
	 (file "/home/jkominek/forecasting/utils.rkt"))

(define opinion-database
  (make-parameter (make-hash)))

(define (load-opinion-database path)
  (read (open-input-file path)))

(define (have-opinion? id #:opinion-database [opinion-database (opinion-database)])
  (hash-has-key? opinion-database id))

(define (get-opinion id #:opinion-database [opinion-database (opinion-database)])
  (hash-ref opinion-database id))

(define (get-all-opinions #:opinion-database [opinion-database (opinion-database)])
  (hash-keys opinion-database))

(struct opinion [for-id beliefs strength settlement]
	#:constructor-name make-opinion)

(define (wrapped-opinion-beliefs o)
  (let ([v (opinion-beliefs o)])
    (normalize-probabilities
     (if (procedure? v)
	 (v)
	 v))))

(define (wrapped-opinion-strength o)
  (let ([v (opinion-strength o)])
    (if (procedure? v)
	(v)
	v)))

(define (wrapped-opinion-settlement o)
  (let ([v (opinion-settlement o)])
    (if v
        (map (lambda (x) (if (procedure? x)
                             (x)
                             x))
             v)
        v)))

(provide (rename-out [wrapped-opinion-beliefs opinion-beliefs]
		     [wrapped-opinion-strength opinion-strength]
                     [wrapped-opinion-settlement opinion-settlement]))

(define (o id beliefs
           [strength 1.0]
           #:settlement [settlement #f])
  (when settlement
    (unless (= (length (if (procedure? beliefs)
                           (beliefs)
                           beliefs))
               (length settlement))
      (printf "belief/settlement mismatch on ~a~n" id)))

  (when #t
    (hash-set! (opinion-database)
               id
               (make-opinion
                id
                (if (procedure? beliefs)
                    beliefs
                    (lambda () beliefs))
                (if (procedure? strength)
                    strength
                    (lambda () strength))
                (if settlement
                    (map (lambda (d)
                           (if (string? d)
                               (date->seconds (read-iso8601 d))
                               d))
                         settlement)
                    settlement)))))

; settlement range
(define (sr start later #:frac [frac 0.5])
  (let ([beginning (if (equal? start 'now)
                       (current-seconds)
                       (let ([provided (date->seconds (read-iso8601 start))])
                         (if (< provided (current-seconds))
                             (current-seconds)
                             provided)))]
        [later (date->seconds (read-iso8601 later))])
      (if (< later beginning)
          later
          (+ beginning (* frac (- later beginning))))))
  
(define (sr-bands dividers #:fracs [raw-fracs #f])
  (define fracs (if raw-fracs
                    raw-fracs
                    (make-list (length dividers) 0.5)))
  (append (for/list ([start dividers]
                     [stop (cdr dividers)]
                     [frac fracs])
            (sr start stop #:frac frac))
          (list (last dividers))))

(define (yes-sooner deadline #:frac [frac 0.5])
  (let ([end (date->seconds (read-iso8601 deadline))]
        [now (current-seconds)])
    (if (< end now)
        (list end end)
        (list end
              (+ now (* (- end now) frac))))))
(define (no-sooner deadline #:frac [frac 0.5])
  (let ([v (yes-sooner deadline #:frac frac)])
    (list (cadr v) (car v))))

(define include-unsafe (make-parameter #t))

(define (ou id beliefs [strength 1.0])
  (when (include-unsafe)
    (o id beliefs strength)))

(define weak 0.5)
(define strong 2.0)

(define (yes p)
  (let ([100-p (- 100 p)])
    (list 100-p p)))

(define (no p)
  (let ([100-p (- 100 p)])
    (list p 100-p)))

(define (linear-scale v #:lo lo #:hi hi)
  (let ([x (/ (- v lo) (- hi lo))])
    (list (- 1 x) x)))

(define (clamp x lo hi)
  (cond
   [(< x lo) lo]
   [(> x hi) hi]
   [else x]))

(define (goes-to f initial
		 #:start start #:stop stop
		 #:flat-before [flat-before #t])
  (let ([start (date->seconds
		(if (string? start)
		    (read-iso8601 start)
		    start))]
	[stop  (date->seconds
		(if (string? stop)
		    (read-iso8601 stop)
		    stop))])
    (lambda ()
      (if (and flat-before
	       (< (current-seconds) start))
	  (f initial)
	  (let ([scale (/ (- (current-seconds) start)
			  (- stop start))])
	    (f (clamp (+ (* (- 100 initial) scale) initial)
                      0 100)))))))

(define (varying-strength start stop
			  #:initial-strength [initial weak]
			  #:final-strength [final strong])
  (let ([start (date->seconds
		(if (string? start)
		    (read-iso8601 start)
		    start))]
	[stop  (date->seconds
		(if (string? stop)
		    (read-iso8601 stop)
		    stop))])
    (lambda ()
      (let ([scale (/ (- (current-seconds) start)
		      (- stop start))])
	(+ (* (- final initial) scale) initial)))))

(define (primary-option option true-prob false-probs)
  (let ([corrected-false
         (map (lambda (p) (* p (- 1.0 true-prob)))
              (normalize-probabilities false-probs))]
        [true-probs
         (build-list (length false-probs)
                     (lambda (i)
                       (if (= i option)
                           true-prob
                           0.0)))])
    (map + corrected-false true-probs)))




(provide opinion-database load-opinion-database have-opinion? get-opinion get-all-opinions include-unsafe)

; Which of the following will be found to contain graph non-isomorphism by 1 Jan 2018?
(o 8 '[1 3 9])

; Will the internet usage worldwide exceed 45% of the world's population before Jan 01, 2015?
(o 18 (yes 10))

; Will Australia reduce its greenhouse gas emissions unconditionally by 5% below their 2000 levels by January 01, 2020?
(o 21 (yes 5))

; When will the Unique Games Conjecture be proven?
(o 25 '[0 5 10 20 30]
   #:settlement
   (sr-bands (list 'now "2015-01-01" "2020-01-01" "2030-01-01" "2050-01-01")
             #:fracs '(0.8 0.75 0.7 0.65)))

; Will a proof be published by 2050 showing Matrix Multiplication in O(n^{2+?}) time, for every constant ? > 0?
(o 28 (yes 66) #:settlement (yes-sooner "2050-01-01"))

; When will the existance of a strongly polynomial-time algorithm for linear programming be proven?
(o 36 '[0.1 5 10 20]
   #:settlement
   (sr-bands (list 'now "2015-01-02" "2020-01-02" "2030-01-01")
             #:fracs '(0.8 0.75 0.7)))

; Will there be a 50%-effective malaria vaccine available for general use before 2015?
(o 84 (yes 0.01)
   #:settlement (yes-sooner "2015-01-01" #:frac 0.9))

; When will a method store qubit superposition states indefinitely room temp less than 50 percent data loss?
(o 123 '[0 0.0 1 20]
   #:settlement (sr-bands (list 'now "2014-07-01" "2014-12-31" "2015-07-01")))

; Will a NASA astronaut be transported to the International Space Station in a commerical spacecraft by DEC 1 2017?
(o 132 '[5 6]
   #:settlement (yes-sooner "2017-12-01" #:frac 0.8))

; Will the International Space Station be abandoned at any point before JAN 1 2022?
(o 134 (yes 33) #:settlement (yes-sooner "2022-01-01"))

; Who will win the DARPA Robotics Challenge Finals in 2015?
 ;(o 140 '[0.01 10 10 13 10 11 10 10 11])
(o 369 '[0.01 10 10 13 10 11 10 10 11])

; Will a paper describing Andrea Rossi's alleged LERN device by published in a journal before Jan 1 2015?
(o 144 (yes 0.01)
   #:settlement (yes-sooner "2015-01-01" #:frac 0.8))

; Will nano structured batteries and super capacitors be incorporated in the bodywork of an electric vehicle available to consumers before the end of 2019?
(o 228 (yes 50) weak #:settlement (yes-sooner "2020-01-01"))

; Will the US National Security Agency build a gate model quantum computer before the end of 2017?
(o 437 (yes 50) weak
   #:settlement (yes-sooner "2017-12-31" #:frac 0.9))

; When will be the first reported occurrence of fraud by an individual passing off an AAM-produced item as a genuine antique of value?
(o 547 '[0 9 12 14 14 14 12]
   #:settlement (sr-bands (list 'now
                                "2015-01-01" "2016-01-01"
                                "2017-01-01" "2018-01-01"
                                "2019-01-01" "2020-01-01")))

; #548 "When will the first artificial internal organ created using AAM technology be successfully transplanted into a human?"
(o 548 '[0 0.9 2 3 4 5 50]
   #:settlement (sr-bands (list 'now "2015-01-01" "2016-01-01" "2017-01-01"
                                "2018-01-01" "2019-01-01" "2020-01-01")))

; When will operation of AAM systems be considered a unique trade skill requiring specific licensing as defined by the National Council of Examiners for Engineering and Surveying (NCEES)?
(o 549 '[0 1.8 3 4 5 6 7]
   #:settlement (sr-bands (list 'now "2015-01-01" "2016-01-01" "2017-01-01"
                                "2018-01-01" "2019-01-01" "2020-01-01")))

; Seagate?s Heat Assisted Magnetic Recorded Drive (HAMR) technology, patented in 2006, could double data-storage density to 1 terabit per square inch. When will the first HAMR be available for purchase by the general public?
(o 565 '[0 0 0 0 0.9 2 3 50]
   0
   #:settlement (sr-bands (list "2014-01-01" "2014-06-30"
                                "2014-09-30" "2014-12-31"
                                "2015-03-31" "2015-06-30"
                                "2015-09-30" "2015-12-31")))

; When will the first primate be cloned?
(o 582 '[0 6 11 15 52]
   #:settlement (sr-bands (list 'now
                                "2015-01-01"
                                "2016-01-01"
                                "2017-01-01"
                                "2018-01-01")))

; When will commercial production of a microprocessor on a 450 millimeter silicon wafer begin?
(o 637 '[0.8 2 3 4 5 10]
   -0.01
   #:settlement (sr-bands (list 'now "2017-01-01" "2020-01-01" "2023-01-01" "2026-01-01" "2029-01-01")))

; When will the first reported collision occur between an autonomous vehicle and a human driven vehicle on a public road? 
(o 638 '[16 4 2 1 .3]
   #:settlement (sr-bands (list 'now "2020-12-31" "2024-12-31" "2028-12-31" "2032-12-31")))

; When will reseachers announce the conversion of methane to methanol at room temperature through the use of metal-organic frameworks?
(o 722 '[0 1.1 3 4 5 6 7 8 9 9]
   #:settlement (sr-bands (list 'now
                                "2015-01-01" "2015-06-30"
                                "2016-01-01" "2016-06-30"
                                "2017-01-01" "2017-06-30"
                                "2018-01-01" "2018-06-30"
                                "2019-01-01")))


; Will NASA's Cold Atom Lab successfully achieve a temperature of 100 picokelvin? 
(o 322 (yes 50))

; Will Jupiter's Great Red Spot shrink below 9000 miles in diameter before January 1 2016?
(o 684 (goes-to no 84.5 #:start "2014-04-01" #:stop "2015-11-15")
   #:settlement (yes-sooner "2016-01-01" #:frac 0.8))

;stable transactinide
(o 678 '(1 1 1 1 1
	   1 1 1 1 1
	   1 1 1 0.01 0.1
	   0.01 0.01 0.01 0.01 0.01
	   0.01 0.01 0.01 0.001 27)
   #:settlement
   (let ([dl "2017-01-01"])
     (list      (sr 'now dl #:frac 0.5)      (sr 'now dl #:frac 0.5)
                (sr 'now dl #:frac 0.5)      (sr 'now dl #:frac 0.5)
                (sr 'now dl #:frac 0.5)      (sr 'now dl #:frac 0.6)
                (sr 'now dl #:frac 0.6)      (sr 'now dl #:frac 0.6)
                (sr 'now dl #:frac 0.6)      (sr 'now dl #:frac 0.6)
                (sr 'now dl #:frac 0.7)      (sr 'now dl #:frac 0.7)
                (sr 'now dl #:frac 0.7)      (sr 'now dl #:frac 0.7)
                (sr 'now dl #:frac 0.7)      (sr 'now dl #:frac 0.8)
                (sr 'now dl #:frac 0.8)      (sr 'now dl #:frac 0.8)
                (sr 'now dl #:frac 0.8)      (sr 'now dl #:frac 0.8)
                (sr 'now dl #:frac 0.9)      (sr 'now dl #:frac 0.9)
                (sr 'now dl #:frac 0.9)      (sr 'now dl #:frac 0.9)
                dl)))

; Will a wearable camera with speed of more than 60 frames per second at 4K resolution be commercially available by the end of 2015?
(o 400 (yes 55) (* 2 strong)
   #:settlement (yes-sooner "2015-12-31" #:frac 0.6))

; Will the scientists at the Harvard-Smithsonian Center for Astrophysics who reported the discovery of gravitational waves be awarded a Nobel Prize in Physics for this work within the next 5 years?
(o 406 (goes-to no 90 #:start "2014-04-01" #:stop "2018-10-15")
   strong
   #:settlement (list "2017-10-15"   ; no
                      "2016-10-15")) ; yes

; Will implantable sensors for monitoring cancer enter clinical trials by the end of 2015?
(o 411 (goes-to no 66 #:start "2014-09-01" #:stop "2015-12-15")
   #:settlement (yes-sooner "2015-12-31"))

; When will the first Watson-based app be available for sale to the public on the Apple iOS operating system?
(o 413 '[0 1 5 10 20] weak)

; When will 3D printing Oreo technology become readily available to the public?
(o 414 '[0 1 10 100 1000 1000] weak)

; Will a dishonest Bitcoin mining pool successfully execute a 51% attack by December 31 2015?
(o 342 (goes-to no 94 #:start "2014-10-01" #:stop "2015-12-01")
   #:settlement (yes-sooner "2015-12-31"))

; Will a cellphone that charges itself using self-charging power cells (SCPC) be demonstrated publicly before the end of 2015?
(o 427 (goes-to no 55 #:start "2015-01-01" #:stop "2015-12-15")
   #:settlement (yes-sooner "2015-12-31"))

; Will DARPA's Mobile Hotspot program enter Phase 3 by the end of June 2015?
(o 681 (yes 20) weak
   #:settlement (yes-sooner "2015-06-30"))

; When will a transactinide isotope with a half-life longer than 29 hours be discovered?
(o 680 '(0.8 1.5 2 2.5 3) strong
   #:settlement (sr-bands (list 'now "2015-12-31" "2017-12-31" "2019-12-31" "2021-12-31")))

; How fast will the next 100 meter human sprinting world record be?
; 2015 world championship in athletics is in beijing, ending sept 6th 2015
; unlikely (impossible?) any record could be set in 2015 after that.
(o 666 '[0.1 0.11 0.12 0.15 1 5] (varying-strength "2014-09-01" "2015-09-06")
   #:settlement (list "2015-09-06"
                      "2015-09-06"
                      "2015-09-06"
                      "2015-09-06"
                      (sr 'now "2015-09-06")
                      "2016-01-01"))

; When will the Deque Conjecture be proven?
(o 26 '[0 9.5 10 10 20]
   #:settlement (sr-bands (list 'now "2015-01-01" "2020-01-01" "2030-01-01" "2050-01-01")))

; How many female chess players will hold a rating of at least 2700 on JUL 01 2016?
(o 318 '[10 5 1 0.01 0.001] strong)

; Will the International Sun-Earth Explorer 3 start to collect data and send it to Earth?
;(o 644 (yes 0))

; Will NASA land a telepresence robot on Venus by 2020?
(o 653 (no 100) (* 2 strong)
   #:settlement (list "2020-01-01" "2019-01-01"))

; When will UHD programming be available from Netflix on optical disc?
(o 587 '[0 1 8 90]
   #:settlement (sr-bands (list 'now "2014-12-31" "2015-06-30" "2015-12-31")
                          #:fracs '(0.95 0.8 0.5)))

; When will a metropolitain area of over 250000 people wirelessly network all traffic signals and sensors so that an AI system can optimize 2-dimensional traffic flow in real-time?
(o 636 '(1 10 9 8 7 6)
   0
   #:settlement (sr-bands (list 'now "2018-12-31" "2022-12-31" "2026-12-31"
                                "2030-12-31" "2034-12-31")))

; Will a solar powered Apple laptop with photovoltaic cells located in the display module be commercially available before the end of 2018?
(o 330 (no 99.9) strong
   #:settlement (yes-sooner "2018-12-31" #:frac 0.8))

; Space agencies for the United States, the European Union, the Russian Federation, and China have announced “plans for plans” for manned missions to Mars, with target landing dates in the 2030s. When will a human being actually set foot on the surface of Mars?
(o 532 (linear-scale 2035 #:lo 2014 #:hi 2036)
   #:settlement (no-sooner "2036-01-01" #:frac 0.9))

; Will quantum key distribution be integrated into a prototype mobile device by 1 January 2016?
(o 562 (no 80)
   #:settlement (yes-sooner "2016-01-01" #:frac 0.6))

; When will the first chess player achieve a rating of at least 2900?
(o 313 '[0 5 6 4 3]
   #:settlement (sr-bands (list 'now "2014-12-31" "2015-12-31"
                                "2016-12-31" "2017-12-31")))

; Will Paypal integrate Bitcoin payments by February 28 2015?
(o 341 (goes-to no 90 #:start "2014-10-01" #:stop "2015-02-28") 0
   #:settlement (yes-sooner "2015-02-28" #:frac 0.6))

; Will Google integrate Bitcoin payments into Google Wallet by February 28 2015?
(o 338 (goes-to no 90 #:start "2014-10-01" #:stop "2015-02-28") weak
   #:settlement (yes-sooner "2015-02-28"))

; Will either the United States or China be able to build an encryption cracking quantum computer before the end of 2018?
(o 436 (yes 15)
   #:settlement (yes-sooner "2018-12-31" #:frac 0.8))

; Will additional evidence of waves in Titan's seas be observed in new images by the end of the Cassini mission? 
(o 456 (yes 40) strong
   #:settlement (yes-sooner "2017-12-31" #:frac 0.6))

; Will an orbiting body system be discovered in space before the end of 2019 which could verify at least one of the 13 new three-body problem solution families recently discovered by two Belgrade physicists?
(o 442 (no 90) strong
   #:settlement (yes-sooner "2019-12-30" #:frac 0.6))

; Will quantum key distribution be integrated into a commercially available mobile device by 1 January 2017?
(o 534 (no 90) (* 2.75 strong)
   #:settlement (yes-sooner "2017-01-01" #:frac 0.75))

; When will the first mass-produced multicopter, for human flight, be offered for sale?
(o 182 '[0 2 8 18]
   #:settlement (sr-bands (list 'now "2015-01-01" "2017-01-01" "2020-01-01")))

; Will a breathalyzer for measuring blood sugar be commercially available by the end of 2016?
(o 402 (yes 30) weak
   #:settlement (yes-sooner "2016-12-31"))

; Will a solar cell with efficiency of at least 50% be reported by the end of 2017?
(o 424 (no 66)
   #:settlement (yes-sooner "2017-12-31"))

; What kind of a computing device will be used to create the first 8-man chess endgame tablebase?
(o 312 '[1 100 1 1 10] (varying-strength "2014-01-01" "2020-01-01"
					#:initial-strength (/ weak 4)
					#:final-strength strong)
   #:settlement (list (sr 'now "2025-01-01" #:frac 0.9)
                      (sr 'now "2025-01-01")
                      (sr 'now "2025-01-01" #:frac 0.75)
                      (sr 'now "2025-01-01" #:frac 0.4)
                      "2025-01-01"))

; When will the Clay Mathematics Institute announce a winner of the Millennium Prize for resolving the "P vs NP Problem"?
(o 27 '[0 2 3 4 200]
   #:settlement (sr-bands (list 'now "2015-01-01" "2020-01-01" "2030-01-01" "2050-01-01")))

; Will the silver nanowire ink touch sensitive screens being developed by 3M and Cambrios be in commercially available smartphones by the end of 2015?
(o 363 (yes 33)
   #:settlement (yes-sooner "2015-12-31" #:frac 0.75))

; Will the 9-man chess endgame tablebase be created before JAN 01 2030?
(o 311 (yes 50) (varying-strength "2014-06-01" "2029-01-01")
   #:settlement (yes-sooner "2030-01-01" #:frac 0.8))

; Will the unit price of Chinese solar PV modules fall below 50 cents per watt in the US by the end of 2015?
(o 421 (no 52)
   #:settlement (yes-sooner "2015-12-31" #:frac 0.6))

; Will planet Kepler 62f be found to have water on its surface?
(o 608 (yes (* 1/3 ; chance of water
	       1/3 ; chance they'll look well enough to tell
	       100))
   #:settlement (yes-sooner "2022-12-31" #:frac 0.6))

; Will a commercial building with perovskite solar cell windows be completed by the end of 2018? 
(o 329 (goes-to yes 40 #:start "2014-06-01" #:stop "2018-08-01")
   (varying-strength "2014-06-01" "2018-07-01"
		     #:initial-strength 0.8
		     #:final-strength 1.2)
   #:settlement (yes-sooner "2018-12-31" #:frac 0.6))

; When will the first Hyperloop-like system begin public operations?
(o 181 '[5 22 30 40 50 200]
   #:settlement (sr-bands (list 'now "2020-01-01" "2025-01-01" "2030-01-01" "2035-01-01" "2040-01-01")))

; Will Google's glucose monitoring "smart" contact lenses get FDA approval by the end of 2016?
(o 688 (yes 15) weak
   #:settlement (yes-sooner "2016-12-31" #:frac 0.8))

; Will a foldable tablet computer made with inkjet-printed graphene be commercially available by the end of 2018? 
(o 453 (goes-to no 66.666 #:start "2014-06-15" #:stop "2017-10-01")
   (varying-strength "2014-08-01" "2017-11-01")
   #:settlement (yes-sooner "2018-12-31"))

; July 2015 Top 500 Winner
(let ([tianhe2-wins 0.9])
  ; On November 2014 what will be the cores per socket of the Top 500 winner? 
;  (o 45 (primary-option 2 tianhe2-wins
;                        '[4 15 20 18 5]))

  ; On November 2014 what will be the geographic region of the Top 500 winner? 
;  (o 44 (primary-option 1 tianhe2-wins
;                        '[3 2 1 1 0.1]))

  ; On November 2014 what will be the vendor of the Top 500 winner? 
;  (o 43 (primary-option 4 tianhe2-wins
;                        '[10 14 3 3 2]))

  ; On November 2014 what will be the processor generation of the Top 500 winner? 
;  (o 46 (primary-option 2 tianhe2-wins
;                        '[5 8 20 10 5]))

   (void)
  )

; Will the twin prime conjecture be resolved by 2024?
(o 200 (yes 50)
   #:settlement (yes-sooner "2024-01-01"))

; Will a 100 million digit prime number be found before 2016?
(o 201 (yes 7)
   0
   #:settlement (yes-sooner "2016-01-01" #:frac 0.9))

; Will lab experiments conducted with Josephson junctions prove the existence of axions by the end of 2015?
(o 227 (no 91) #:settlement (yes-sooner "2015-12-31" #:frac 0.75))

; Will evidence of proton decay be reported in the scientific literature by the end of 2017?
(o 508 (no 99) (* 2.9 strong)
   #:settlement (yes-sooner "2017-12-31" #:frac 0.8))

; On NOV 2015 what will be the (performance-weighted) gigaflops per watt of the Top 500?
(o 176 '[90 8 1 .1 .01] (* 2/3 strong))

; On NOV 2015, what will be the (performance-weighted) gigaflops per core of the Top 500?
(o 178 '[0 40 60 0 0] strong)

; when will a fully autonomous (self-driving) car made by a major auto maker drive more than 60 miles in the united states without input from the driver?
(o 696 '[0 3 4 3 2 1] weak
   #:settlement
   (sr-bands (list 'now "2015-01-01" "2015-06-30" "2015-12-31"
                   "2016-06-30" "2016-12-31")))

; On November 2014 what will be the Top 500 performance share by geographic region?
(let* ([jul2014-performance
        (list 124825909 78651505 35330764 17099688
              (+ 2898745 6736440 2502392 2329579 2472259 1031365))]
       [nov2014-performance
        (list 138600751 80071203 45748036 20928331
              (+ 5536205 3137692 4635448 2329579 6629962 1031365 201941))]
       [jun2015-performance
        (map * (list 1.1 1.11 1.08 1.08 1.05)
             nov2014-performance)]
       [nov2015-performance
        (map * (list 1.1 1.11 1.08 1.08 1.05)
             jun2015-performance)]
       [nov2017-performance
        (map * (list 1.1 1.11 1.08 1.08 1.05)
             nov2015-performance)]
       )

  ; nov 2014
  ;(o 48 nov2014-performance)

  ; jun 2015
  ; ???

  ; nov 2015
  (o 58 nov2015-performance)

  ; nov 2017
  (o 59 nov2017-performance)

  ; nov 2020
  (o 63 nov2017-performance)

  ; nov 2030
  (o 67 nov2017-performance)

  ; nov 2050
  (o 69 nov2017-performance)
  )

; On November 2014 what will be the Top 500 performance share by cores per socket?
(let* ([current-performance
        (list (+ 4451142 21457755) 99215856 (+ 23363299 53226459) 72366076 100)]
       [nov2014-performance
        (list (+ 4024611 19951897) 96317311 (+ 34381706 71837501) 77535023 100)]
       [jun2015-performance nov2014-performance]
       [nov2015-performance
        (map * '[0.9 1 1 1.1 1.2]
             jun2015-performance)]
       [nov2017-performance
        (map * '[0.9 1 1 1.1 1.2]
             nov2015-performance)]
       )
  ; nov 2014
  ;(o 56 nov2014-performance)

  ; jun 2015
  ; ???

  ; nov 2015
  (o 49 nov2015-performance)

  ; nov 2017
  (o 60 nov2017-performance)

  ; nov 2020
  (o 64 '[1 2 4 7 14])

  ; nov 2030
  (o 68 '[1 2 4 7 15])

  ; nov 2050
  (o 70 '[1 3 9 27 81])
  )

; On November 2014 what will be the Top 500 performance share by vendor?
(let* ([jul2014-performance (list 87748559 49817954 42817488 10912241 82784343)]
       [nov2014-performance (list 87143814 68198477 44855405 14741773 93911043)]
       [jun2015-performance nov2014-performance]
       [nov2015-performance jun2015-performance]
       [nov2017-performance 
        (map * '[1 1 1.1 0.9 1.1]
             nov2015-performance)]
       )
  ; nov 2014
  ;(o 47 nov2014-performance)

  ; jun 2015
  ; ???

  ; nov 2015
  (o 57 nov2015-performance)

  ; nov 2017
  (o 50 nov2017-performance)

  ; nov 2030
  (o 66 '[10 10 9 1 20])
  )

; On November 2020, what will be the Top 500 performance share by processor generation?
(o 65 '[0.01 0.05 0.1 0.5 10])

; Will Amazon deliver its first package using an unmanned aerial vehicle by DEC 31 2017?
(o 105 (yes 10) weak
   #:settlement (yes-sooner "2016-12-31" #:frac 0.7))

; Will the Mars Curiosity Rover discover organic matter on Mars-evidence that life exists or existed on the planet-by July 1 2015?
(o 377 (no 98.6) #:settlement (yes-sooner "2015-07-01" #:frac 0.333))

; When will the Chinese National Space Administration land a man or woman on the moon?
(o 136 '[1 8 10 15]
   #:settlement (sr-bands (list 'now "2022-01-01" "2024-01-01" "2026-01-01")))

; Will the Chinese National Space Administration retrieve at least 2kg of lunar rock/soil samples by January 1 2018? 
(o 135 (yes 15) (varying-strength "2014-06-15" "2017-10-01")
   #:settlement (yes-sooner "2018-01-01" #:frac 0.6))

; When will floating wind turbines be used in a commercial offshore wind turbine farm?
(o 712 '[9 20 30 40 50 60]
   #:settlement (sr-bands (list 'now "2016-01-01" "2017-01-01"
                                "2018-01-01" "2019-01-01" "2020-01-01")))

; When will a prickless glucose monitor be commercially available to consumers?
(o 709 '[3 6 7 8 7 5]
   #:settlement (sr-bands (list 'now "2016-12-31" "2017-12-31"
                                "2018-12-31" "2019-12-31" "2020-12-31")))

; Will a new clean & jerk world record be set before or during the 2016 Olympic Games?
(o 711 (yes 25) (* 5 strong)
   #:settlement (yes-sooner "2016-08-21" #:frac 0.9))

; Will Huawei confirm allegations that "back doors" were installed on their computer hardware before being sold?
(o 623 (goes-to no 96 #:start "2014-11-01" #:stop "2015-12-24")
   (* 2 strong)
   #:settlement (yes-sooner "2015-12-31"))

; Will a solar-powered plane circumnavigate Earth before the end of 2015?
; http://www.bloomberg.com/news/articles/2015-03-09/solar-powered-plane-takes-off-for-round-the-world-flight-attempt
(o 629 (yes 45)
   #:settlement (yes-sooner "2015-12-31" #:frac 0.75))

; Will Google incorporate infrared vision into future Google Glass models by the end of 2017?
(o 535 (yes 42) 0
   #:settlement (yes-sooner "2017-12-31"))

; Will obesity be among the top five health-related keyword searches in Google in any month in 2014?
(o 188 (goes-to no 97.5 #:start "2014-11-01" #:stop "2014-12-30")
   #:settlement (yes-sooner "2015-01-01"))

; When will a classical algorithm be authored to solve 3-SAT faster than exp((o n))?
(o 32 '[0 1 2 3 6] weak
   #:settlement (sr-bands (list 'now "2015-01-01" "2020-01-01" "2030-01-01" "2050-01-01")))

; Internet of things 20 billion connected devices at end of 2013. how many at end of 2014?
(o 566 '[2 100 170 240 290] (* weak -1.1))

; world produced 4.4 zettabytes in 2013. how much in 2014?
(o 620 '[2 30 80 80 60] (* weak -1.25))

; which smallsat company will be the first to provide daily re-imaging of the earth?
(o 714 '[60 80 10 55 80 50]
   #:settlement (list (sr 'now "2017-12-31")
                      (sr 'now "2017-12-31")
                      (sr 'now "2017-12-31")
                      (sr 'now "2017-12-31")
                      (sr 'now "2017-12-31")
                      "2017-12-31"))

; When the consumer version of Google Glass is released to the general public, what will its price be? 
(o 399 '[10 90 29] 
   0.9
   #:settlement (list "3000-01-01"
                      "3000-01-01"
                      "3000-01-01"))

; Doctors saved the life of a boy suffering from encephalitis when a sequencing test turned up DNA of a lethal bacteria in his cerebrospinal fluid. What type of pathogen will this test discover next in a critically-ill patient suffering from encephalitis? 
#;(o 720 '[4 6 2 3 1 14]
   0
   #:settlement (list (sr 'now "2016-01-01")
                      (sr 'now "2016-01-01")
                      (sr 'now "2016-01-01")
                      (sr 'now "2016-01-01")
                      (sr 'now "2016-01-01")
                      "2016-01-01"))

; Will a Tesla electric car with a base price under $35,000 be for sale in the U.S. before the end of 2016? 
(o 717 (yes 7)
   #:settlement (yes-sooner "2016-12-31" #:frac 0.8))

; pluto's exact size
(o 723 '[1 1]) ; range from 2280km to 2400km

; free-form gestures be used as access passwords in commercial touchscreen mobile devices?
#;(o 724 '[0 7 10 13] strong
   #:settlement (sr-bands (list 'now "2014-12-31" "2015-06-30" "2015-12-31")
                          #:fracs '(0.9 0.8 0.7)))

; high-precision measurement of antiproton's magnetic moment?
(o 725 '[0 10 12 15]
   #:settlement (sr-bands (list 'now "2015-01-01" "2016-01-01" "2017-01-01")))

; MH370 debris
(o 726 '[20 10 33] 0
   #:settlement (list (sr 'now "2016-01-01" #:frac 0.4)
                      (sr 'now "2016-01-01" #:frac 0.5)
                      "2016-01-01"))

; MH370 search days
(o 727 (linear-scale 380 #:lo 0 #:hi 400)
    #:settlement (no-sooner "2016-12-31"))

; When will the traversal conjecture be proven?
(o 29 (list 0 5 10 20 30)
   #:settlement (sr-bands (list 'now "2015-01-01" "2020-01-01" "2030-01-01" "2050-01-01")))

; When will a piloted solar plane fly around the world only on solar energy?
(o 721 '[0.25 4 2.5 2 2 2 1.9] 0.1
   #:settlement (sr-bands (list 'now
                                "2015-06-30" "2015-12-31"
                                "2016-06-30" "2016-12-31"
                                "2017-06-30" "2017-12-31")))

; Which private company will be the first to shuttle an astronaut to the International Space Station? 
(o 133 '[10 5 27 8 39]
   #:settlement (list (sr 'now "2020-01-01")
                      (sr 'now "2020-01-01")
                      (sr 'now "2020-01-01")
                      (sr 'now "2020-01-01")
                      "2020-01-01"))

; When will an article describing the observation of a Population III star be published in a journal indexed by Thomson Reuter’s Web of Science? 
(o 170 '[0 1.9 4 8 32]
   #:settlement (sr-bands (list 'now "2015-01-01" "2016-01-01" "2017-01-01" "2018-01-01")))

; Which organization will be the first to observe a Population III star?
(o 171 '[1 1 1 1 1 1 7.5]
   #:settlement (list (sr 'now "2016-01-01")
                      (sr 'now "2016-01-01")
                      (sr 'now "2016-01-01")
                      (sr 'now "2016-01-01")
                      (sr 'now "2016-01-01")
                      (sr 'now "2016-01-01")
                      "2016-01-01"))

; How many teams will be awarded a full prize in the final round of the Qualcomm Tricorder X Prize competition?
(o 184 '[5 9 13 35] 0.35)

; When will a supersonic aircraft be available again for commercial travel?
(o 320 '[11 8 7 6 5]
   #:settlement (sr-bands (list 'now "2019-12-31" "2020-12-31" "2021-12-31" "2022-12-31")))

; When will the Bitcoin blockchain first register over 150,000 transactions in a single day?
(o 337 '[0 0 0 1 99]
   #:settlement (sr-bands (list 'now "2014-07-31" "2014-10-31" "2015-12-31" "2015-04-30")))

; Will Facebook use drones perhaps high-altitude, solar-powered craft like those made by Titan Aerospace to provide internet access in places where it is not currently accessible, by the end of December 2017?
(o 531 (yes 12.5)
   #:settlement (yes-sooner "2017-12-31"))

; Will the global gender gap in the number of internet users decline in 2014? 
(o 552 (yes 13))

; When will Google or Facebook debut internet connectivity to consumers using solar-powered drones?
(o 553 '[0 30 100 300]
   #:settlement (sr-bands (list 'now "2015-01-01" "2016-01-01" "2017-01-01")))

; What percentage of Japan's electricity generation in 2015 will come from nuclear power? 
(o 555 '[4 5 5 1 .5 .1])

; Will Google or Facebook first debut internet connectivity to consumers using solar-powered drones? 
(o 557 '[11 1 9])

; How many threatened languages will the Ethnologue language catalog report in its 18th edition?
;(o 663 '[1 5 30 50 30])

; How many languages will the Ethnologue language catalog's 18th edition report as spoken by no one as a first language?
;(o 665 '[1.8 3.8 11.6 45.4 33.0 4.4] (* 0.15 weak))

; When will the Bailiwick of Jersey have over 50 companies that accept Bitcoin as a form of payment?
(o 729 '[0 0.7 1.9 11.3]
   #:settlement (sr-bands (list 'now "2014-12-31" "2015-06-30" "2015-12-31")))

; At its initial launch, will the range of Tesla's Gen 3 electric car be greater than 150 miles? 
(o 731 (yes 90)
   #:settlement (yes-sooner "2018-01-01"))

; In June 2014, Amazon unveiled the "Fire", the world's first 3-D smartphone. Which of these companies will be next to release a 3-D smartphone? 
(o 732 '[100 210 190] weak)

; Will physicists find any difference in the magnetic moments of the antiproton and the proton when they achieve a direct high-precision measurement of the antiproton?
(o 734 (yes 25))

; Engineered water nanostructures have been shown to inactivate dangerous pathogens at eight times the natural rate without toxic residue. How much faster than the natural rate will this technology inactivate surface bacteria according to the next report?
(o 736 '[10 9 8 7 6])

; When will a proof be published showing L=RL?
(o 31 '[0 5 10 20 5]
   #:settlement (sr-bands (list 'now
                                "2015-01-01" "2020-01-01"
                                "2030-01-01" "2050-01-01")))

; Will Solar Roadways be installed on any public roadway in the U.S. for public use by 2020?
(o 742 (yes 20)
   0
   #:settlement (yes-sooner "2020-01-01" #:frac 0.75))

; Will researchers observe neutrinoless double-Î² decay to verify the Majorana nature of the neutrino by January 1, 2017? 
(o 746 (yes 25)
   #:settlement (yes-sooner "2017-01-01"))

; A recent analysis of the nitrogen in Titan's atmosphere found the moon's atmospheric nitrogen originated in conditions similar to ancient comets from the Oort cloud. Will New Horizon's analysis of Pluto's atmospheric nitrogen find similar conditions?
(o 745 (yes 66))

; A NASA research team is currently conducting a small, low-budget experiment attempting to create a warp in spacetime. Will NASA's White?Juday warp-field interferometer experiment create a a microscopic instance of a warping of spacetime before 2020?
(o 744 (yes 15))

; When will a proof be published showing graph isomorphism is solvable in classical polynomial time?
(o 24 '[0 5 10 20 30] weak
   #:settlement (sr-bands (list 'now
                                "2015-01-01" "2020-01-01"
                                "2030-01-01" "2050-01-01")))

; What will be the average rating of the top 10 chess players in the world on JAN 01 2016? 
(o 314 '[1 120 18] strong)

; Will the U.S. be the world leader in total oil production in 2015?
(o 484 (yes 70) strong)

; If the icy surface of Pluto's giant moon Charon is cracked, analysis of the fractures could reveal if its interior was warm, perhaps warm enough to have maintained a subterranean ocean of liquid water. Will researchers observe cracks in Charon's surface?
(o 740 (yes 40))

; When will photonic waveguide sensors being developed by Corning Inc. and Polytechnique Montreal be used in a commercially available smartphone?
(o 749 '[0 1 2 3 6]
   #:settlement (sr-bands (list 'now "2014-12-31" "2015-12-31" "2016-12-31" "2017-12-31")))

; "A 500-meter loop of elevated networked sky cars is planned to be built in Tel Aviv. When will it carry its first passenger?"
(o 755 '[0 .9 14 20 13 9 4 3 2 20]
   #:settlement (sr-bands (list 'now
                                "2015-01-01" "2015-06-30"
                                "2016-01-01" "2016-06-30"
                                "2017-01-01" "2017-06-30"
                                "2018-01-01" "2018-06-30"
                                "2019-01-01")))

; How tall will Kingdom Tower in Saudi Arabia be when completed?
(o 756 (linear-scale 1009 #:lo 999.0 #:hi 1060.0))

; Will a smartphone that incorporates a temperature sensor in the glass be publically available by Jan 1 2017?
(o 757 (yes 10) (* 1.5 strong)
   #:settlement (yes-sooner "2017-01-01" #:frac 0.8))

; When will a handheld, flexible nanocellulose video screen be comercially available for sale?
(o 759 '[1 2 2 3 3 4 9]
   #:settlement (sr-bands (list 'now
                                "2015-04-30" "2015-10-31" "2016-04-30"
                                "2016-10-31" "2017-04-30" "2017-10-31")))

; Which of the following battery types mentioned in the C&EN July 2014 issue will be the first to be used in an electric car sold by 2020?
(o 760 '[4 2 4 2 1.5])

; When will the pharmaceutical industry produce a new drug to aid the deaf, by targeting the inner ear?
(o 762 '[1.5 2 2 3 3 2]
   #:settlement (sr-bands (list 'now
                                "2017-01-01" "2019-01-01"
                                "2021-01-01" "2024-01-01"
                                "2028-01-01")))

; "Will SpaceX successfully complete the first manned test flight of its Dragon V2 spacecraft before June 30, 2016?"
(o 768 (yes 52) weak
   #:settlement (yes-sooner "2016-06-30" #:frac 0.75))

; "Will Boeing successfully complete a manned test flight of its CST-100 spacecraft before the end of June 2017?"
(o 769 (yes 52) weak
   #:settlement (yes-sooner "2017-06-30" #:frac 0.75))

; "Will Sierra Nevada successfully complete a manned test flight of its Dream Chaser spacecraft before the end of June 2017?"
(o 770 (yes 30) weak
   #:settlement (yes-sooner "2017-06-30" #:frac 0.75))

; "Which company's manned 'space taxi' will be used first by NASA to transport astronauts to the International Space Station?"
(o 771 '[120 80 8 20])

; "Will a NASA-funded 'space taxi' transport astronauts to the International Space Station before the end of 2017?"
(o 772 (yes 10) strong
   #:settlement (yes-sooner "2017-12-31" #:frac 0.8))

; When will a brain computer interface (BCI) that can translate brain signals to audible speech be commercially available?
(o 799 '[1 1.1 2 3 15]
   #:settlement (sr-bands (list 'now
                                "2016-12-31" "2017-12-31"
                                "2018-12-31" "2019-12-31")))

; By the end of 2015, will there be more than 2,500 CHAdeMO quick charging stations for battery electric vehicles in Japan?
#;(o 801 (yes 75)
   0
   #:settlement (yes-sooner "2015-12-31" #:frac 0.75))

; By the end of 2015, will there more than 1,000 CHAdeMO quick charging stations for battery electric vehicles in the U.S.?
#;(o 802 (yes 71)
   #:settlement (yes-sooner "2015-12-31" #:frac 0.75))

; By the end of 2015, will there be more than 5,000 CHAdeMO quick charging stations for battery electric vehicles in the world?
#;(o 803 (yes 65) 0
   #:settlement (yes-sooner "2015-12-31" #:frac 0.75))

; When will the world's first quantum key distribution satellite become operational?
(o 805 '[0 .1 1 2 2 2 2 10]
   #:settlement (sr-bands (list 'now
                                "2015-01-01" "2015-06-30"
                                "2016-01-01" "2016-06-30"
                                "2017-01-01" "2017-06-30"
                                "2018-01-01")))

; Which organization(s) will develop the first operational space-based quantum key distribution transmitter?
(o 806 '[10 20 15 5 15])

; When will resistive random access memory (RRAM) be used in a commercially available smartphone?
(o 812 '[0 100 200 300 400] (* 1.2 strong)
   #:settlement (sr-bands (list 'now
                                "2014-12-31" "2015-12-31"
                                "2016-12-31" "2017-12-31")
                          #:fracs '(0.95 0.8 0.7 0.6)))

; When Google Glass is released to the general public, what will its intial price be?
(o 822 (linear-scale 850 #:lo 300 #:hi 1200) 0.8
  #:settlement (list "3000-01-01" "3000-01-01"))

; When will a distinct source of ultrahigh-energy cosmic rays be discovered?
(o 824 '[2.4 1 1 1 5]
   #:settlement (sr-bands (list 'now
                                "2017-01-01" "2018-01-01"
                                "2019-01-01" "2020-01-01")))

; Singlet fission transforms an excited singlet state into a pair of triplet excitons and can potentially boost the efficiency of solar energy conversion. Will an inorganic solar cell exceed the Shockley-Queisser limit of 33% using singlet fission by 2017?
(o 825 (yes 10)
   #:settlement (yes-sooner "2016-12-31"))

; When will the first product containing a microchip that uses nanotube transistors go on sale to the general public?
(o 826 '[1 10 100 200 300 300]
   #:settlement (sr-bands (list 'now "2019-01-01" "2020-01-01"
                                "2021-01-01" "2022-01-01" "2023-01-01")))

; Will the ALPHA and AEgIS experiments conducted by CERN indicate that the ratio of gravitational mass to inertial mass of antihydrogen is positive or negative?
(o 827 '[1000 1 1900])

; Will NASA's measurement of thrust from an 'impossible' space drive be reproduced on the next try?
(o 837 (yes 16) #:settlement (yes-sooner "2015-12-31"))

; Will Yahoo Offer End-to-End (E2EE) Encryption as a Feature in Yahoo Mail by June 30, 2015?
(o 842 (yes 41) weak #:settlement (yes-sooner "2016-06-30"))

; How many base pairs will the smallest genome discovered by the end of 2016 contain?
(o 843 (linear-scale 107000 #:lo 100000 #:hi 112090))

; When will the first product containing a neuromorphic microchip go on sale to the general public?
(o 844 '[5 6 7 8 9 10]
   #:settlement (sr-bands (list 'now "2017-12-31" "2018-12-31" "2019-12-31"
                                "2020-12-31" "2021-12-31")
                          #:fracs '(0.9 0.8 0.7 0.6 0.5)))

; How many unclassified languages will the Ethnologue language catalog report in its 18th edition?
;(o 845 '[100 5 5 5 100] weak)

; What team will score the highest in NIST's 2015 OpenMT Arabic-To-English machine translation evaluation?
(o 846 '[90 100 100 100 80 60 80 90 60 60 100 100 70])

; What team will score the highest in NIST's 2015 OpenMT Chinese-To-English machine translation evaluation?
(o 847 '[90 100 90 100 80 80 70 80 90 80 100 80 100 100 90 80 70])

; Will the NIST 2015 OpenMT Arabic-To-English machine translation evaluation be a split decision?
(o 848 (yes 40) weak)

; When will a wireless pacemaker be commercially available in the United States?
(o 850 '[5 6 6 6]
   #:settlement (sr-bands (list 'now "2015-12-31" "2016-12-31" "2017-12-31")))

; What will the unemployment rate for ACS chemists be in 2015?
(o 851 (linear-scale 3.0 #:lo 0.0 #:hi 4.7))

; Windows 10 available before April 30 2015?
(o 861 (no 99.999)
   #:settlement (yes-sooner "2015-04-30" #:frac 0.9))

; Windows 10 64-bit only?
(o 863 (yes 0.75))

; Microsoft free Windows 10 to 8 users?
(o 864 (yes 4))

; Microsoft free Windows 10 to XP Vista and 7 users?
(o 865 (yes 2))

; Microsoft Windows 10 include Kinect-based 3D gesture?
(o 866 (yes 12) weak)

; Millimeter scale programmable matter
(o 868 '[0 1.9 5 7 9 11 18]
   #:settlement (sr-bands (list 'now "2015-01-01" "2016-01-01" "2017-01-01"
                                "2018-01-01" "2019-01-01" "2020-01-01")))

; Did hackers gain access to the personal data of celebrities via a vulnerability in Apple's Find My iPhone service?
(o 894 (yes 0.01) strong)

; When will the first car equipped with vehicle-to-vehicle (V2V) safety technology be offered for sale to the general public in the US?
(o 900 '[2 8 18 40])

; By the end of 2016, will a compact quantum random number generator be commercially available in the US?
(o 901 (goes-to no 68 #:start "2014-12-01" #:stop "2016-12-01"))

; Will the European Space Agency's Rossetta mission find any organic compounds in the cometary dust it collects from Comet 67P/C-G?
   ;(o 902 (yes 55) (* 2 strong))

; When will a car company first debut a production model of a car capable of vehicle to vehicle (V2V) communication and vehicle to infrastructure (V2I) communication?
(o 909 '[0.4 2 5 10 20])

; Will a private organization put a human on Mars before NASA does?
(o 912 (yes 50))

; Before the start of 2020, will a drug that reverses diabetes be approved by the FDA for use in the US?
(o 914 (yes 20) #:settlement (yes-sooner "2020-01-01"))

; Which automobile manufacturer will win the Best Concept Vehicle award for the 11th Annual EyesOn Design Competition at the 2015 NAIAS?
#;(o 918 '[10 11 10 8 8 3 11 7 7 7 4 6 4 7 4 7 3] -0.001)

; Today's brain-computer interfaces (BCIs) are research projects or novelty products. When will a BCI first be incorporated into a commercially available, wearable device allowing an average user to reliably control standard software or a major device?
(o 919 '[1 3 5 7 9])

; When will a display using nanorods be commercially available?
(o 921 (map *
            '[1 2 3 4]
            '[1.25 1 1 3]))

; Will Open Garden offer an encryption feature for its peer-to-peer networking app FireChat by the end of 2014?
(o 937 (goes-to no 70 #:start "2014-10-01" #:stop "2014-12-28")
   0.9
   #:settlement (yes-sooner "2015-01-01" #:frac 0.9))

; Will Tesla's new Gigafactory meet its projected production level of 35 gigawatt-hours of batteries per year by 2020?
(o 992 (yes 60) #:settlement (yes-sooner "2020-01-01" #:frac 0.8))

; Paleontologists have recently unveiled the nearly complete skeleton of a large, predatory, semi-aquatic dinosaur. Will second new species of dinosaur with adaptations for aquatic environments be discovered before 2017?
(o 993 (goes-to no 60 #:start "2014-09-01" #:stop "2018-01-01") weak #:settlement (yes-sooner "2017-01-01"))

; Which form of grid level energy storage, other than pumped hydro, will have the largest contribution to the US energy grid by the end of 2015?
(o 994 '[275 20 10 35 25] strong)

; When will the first professional sports team use on-body biosensors during an actual sports contest?
(o 995 (map *
            '[1.2 1 1 2]
            '[1 2 2 1])
   #:settlement (sr-bands (list 'now "2016-01-01" "2017-01-01" "2018-01-01")))

; When will a qubit processor be demonstrated that has both fidelity above 99% and at least 512 qubits?
(o 996 '[0 50 100 200]
   #:settlement (sr-bands (list 'now "2015-01-01" "2016-01-01" "2017-01-01")))

; Liu et al. reported on 8/27/2014 their discovery and synthesis of a new class of multiferroics that may lead to massive storage capacities for small memory sticks. Will a research team demonstrate a flash-memory device using these multiferroics by 2017?
(o 997 (yes 15) #:settlement (yes-sooner "2017-01-01"))

; When will a memristor-based computer chip that can mimic the human brain's computing capability be available in the marketplace?
(o 999 '[0 0 0 1 100]
   #:settlement
   (sr-bands (list 'now "2016-01-01" "2017-01-01" "2018-01-01"
                   "2019-01-01")))

; When will a manned submarine achieve speeds at or above 100 knots underwater using supercavitation?
(o 1002 '[0 .9 2 3 4 5 6 5]
   #:settlement
   (sr-bands (list 'now
                   "2015-01-01" "2016-01-01"
                   "2017-01-01" "2018-01-01"
                   "2019-01-01" "2020-01-01"
                   "2021-01-01")))

; Physicists from the University of Texas-Arlington have recently published a paper describing a new method of detecting moons orbiting exoplanets by using radio waves. Will an exomoon be discovered using this technique before 2020?
(o 1003 (yes 66) #:settlement (yes-sooner "2020-01-01"))

; Will a hospital purchase a paraxial ray optics based cloaking apparatus for use during surgery by the end of 2015?
(o 1006 (goes-to no 90 #:start "2014-10-21" #:stop "2015-12-01")
   #:settlement (yes-sooner "2015-12-01" #:frac 0.8))

; Will a smartphone with an antibacterial glass panel containing iron nanoparticles be commercially available before the end of 2016?
(o 1020 (goes-to no 70 #:start "2014-10-01" #:stop "2018-01-01")
   #:settlement (yes-sooner "2016-01-01" #:frac 0.6))

; Will a lab build a 20 tesla bore field in a superconducting dipole magnet by 2020?
(o 1024 (yes 33) #:settlement (yes-sooner "2020-01-01") strong)

; Will there be a confirmed case of Ebola where symptoms appeared 21 days after exposure?
(o 1026 (goes-to no 69.5 #:start "2014-10-21" #:stop "2015-03-01") #:settlement (yes-sooner "2015-03-19"))

; Which technology focus will win the Nokia Sensing XCHALLENGE $525,000 grand prize for a senor-based device that individuals use to access, understand, and improve their health and well-being?
;(o 1028 '[10 5 8 4] (* 0.5 weak))

; Will the Steam Machine gaming hardware platform be officially released before the end of March 2015?
(o 1029 (goes-to no 94 #:start "2015-02-01" #:stop "2015-03-31")
   0
   #:settlement (yes-sooner "2015-03-31" #:frac 0.75))

; Will LG unveil a smartphone with a 4K display at the Mobile World Congress in March 2015?
(o 1040 (yes 0.9) (* 0.475 weak)
   #:settlement '("2015-03-20" "2015-03-03"))

; Will the World Health Organization declare the current Ebola outbreak to be over in West Africa by the end of March 2015?
(o 1043 (yes 0.75) weak)

; Will the implant of an osseointegrated (i.e. bone-anchored) prosthetic device in an amputee in the US be reported before the end of March 2015?
(o 1044 (goes-to no 58 #:start "2015-01-01" #:stop "2015-04-03")
   weak)

; When will humans establish a permanent human base on the Moon?
(o 1048 '[1 2 100] 0 #:settlement (sr-bands (list 'now "2022-12-31" "2039-12-31")))

; When will Apple Watch be available for sale?
  ; announcement on mar 9th
#;(o 1050
   '[0.001 .01 20 150 35 10 5 2.5 1]
   #:settlement (sr-bands (list "2015-01-01" "2015-02-01"
                                "2015-03-01" "2015-04-01"
                                "2015-05-01" "2015-06-01"
                                "2015-07-01" "2015-08-01"
                                "2015-09-01")))

; Baek et al. published in September 2014 that they found that electromagnetism promotes pluripotency. Will a second research group publish a paper confirming the increase efficiency in iPS due to electromagnetic fields before March 31, 2015?
(o 1051 (goes-to no 90 #:start "2015-02-15" #:stop "2015-03-31")
   #:settlement (yes-sooner "2015-03-31"))

; Will Apple Watch be available for sale by March 31, 2015?
;(o 1052 (goes-to no 90 #:start "2015-02-20" #:stop "2015-04-01")
;   #:settlement (yes-sooner "2015-03-31" #:frac 0.8))

; Will facial recognition software with an error rate equal to or less than that exibited by humans in facial recognition tasks (2.47%) be demonstrated before March 31st 2015?
(o 1056 (yes 5) #:settlement (yes-sooner "2015-03-31"))

; Will Li-Fi achieving a transmission rate of 40Gbps be reported by the end of March 2015?
(o 1061 (goes-to no 85 #:start "2015-02-01" #:stop "2015-03-31")
   weak #:settlement (yes-sooner "2015-03-31"))

; At the end of March 2015, will there be 4,700 or more CHAdeMO quick charging stations for battery electric vehicles worldwide?
;(o 1063 (yes 62) weak #:settlement (yes-sooner "2015-03-31" #:frac 0.9))

; Will analysis of the samples collected by Philae, the lander on Comet 67P, show the presence of amino acids in the material collected from the comet by 31 MAR 2015?
(o 1068 (goes-to no 95 #:start "2015-02-08" #:stop "2015-03-31")
   strong #:settlement (yes-sooner "2015-03-31" #:frac 0.9))

; Will NASA lose contact with the Mars Curiosity rover by March 31, 2015?
(o 1073 (goes-to no 70 #:start "2014-11-20" #:stop "2015-03-31")
   #:settlement (yes-sooner "2015-03-31"))

; How many active space-focused projects will Zooniverse be hosting on March 31, 2015? 
(o 1074 '[1.5 9 85 9 1.5])

; Will a new Mersenne prime be discovered by March 31, 2015? 
(o 1075 (goes-to no 85 #:start "2014-11-20" #:stop "2015-03-31")
   #:settlement (yes-sooner "2015-03-31"))

(let ([no-exascale-by-2023 0.25])
  ; When will an exascale supercomputer appear on the Top 500 list?
  (o 1066
     (primary-option 13 no-exascale-by-2023
                     '[ 1  1  1  1  1
                        5 10 10 11 11
                       11 12 13 0])
     #:settlement (sr-bands (list 'now
                                  "2017-06-10" "2017-11-10"
                                  "2018-06-10" "2018-11-10"
                                  "2019-06-10" "2019-11-10"
                                  "2020-06-10" "2020-11-10"
                                  "2021-06-10" "2021-11-10"
                                  "2022-06-10" "2022-11-10"
                                  "2023-01-01")))

  ; What will be the vendor of the first exascale supercomputer on the Top 500 list?
  ; should think about this one
  (o 1076 (primary-option 7 no-exascale-by-2023
                          '[11 3 10 3 10 10 10 0])
     #:settlement (list (sr 'now "2023-01-01" #:frac 0.4)
                        (sr 'now "2023-01-01" #:frac 0.9)
                        (sr 'now "2023-01-01")
                        (sr 'now "2023-01-01" #:frac 0.9)
                        (sr 'now "2023-01-01")
                        (sr 'now "2023-01-01")
                        (sr 'now "2023-01-01")
                        "2023-01-01"))

  ; Where will the first exascale computer on the Top 500 list be built?
  ; should think about this one
  (o 1077 (primary-option 10 no-exascale-by-2023
                          '[1 10 2 10 7 1 2 2 8 1 0])
     #:settlement (list (sr 'now "2023-01-01" #:frac 0.9)
                        (sr 'now "2023-01-01" #:frac 0.3)
                        (sr 'now "2023-01-01" #:frac 0.9)
                        (sr 'now "2023-01-01" #:frac 0.2)
                        (sr 'now "2023-01-01" #:frac 0.5)
                        (sr 'now "2023-01-01" #:frac 0.9)
                        (sr 'now "2023-01-01" #:frac 0.8)
                        (sr 'now "2023-01-01" #:frac 0.8)
                        (sr 'now "2023-01-01" #:frac 0.5)
                        (sr 'now "2023-01-01" #:frac 0.9)
                        "2023-01-01"))
  )

; Everykey is a Bluetooth enabled wristband that can unlock password-protected websites or electronic key-protected devices with the users' login credentials to allow instant access. Will the first shipment of Everykey occur before April 2015?
(o 1085 (goes-to no 99 #:start "2015-02-10" #:stop "2015-03-31")
   (* 2 strong)
   #:settlement (yes-sooner "2015-03-31" #:frac 0.8))

; Will the price of road salt reach $200 per metric ton in any state in the United States before March 2015?
(o 1086 (yes 29) 0.1 #:settlement (yes-sooner "2015-02-28"))

; Will there at least two new confirmed Ebola diagnoses in the US between January 1 and March 31, 2015?
(o 1091 (goes-to no 80 #:start "2014-12-01" #:stop "2015-03-29") 0.3
   #:settlement (yes-sooner "2015-03-31"))

; Will quantum factorization of a semiprime larger than 56,153 (the current record) be reported by the end of March 2015?
   ;(o 1103 (yes (void)) #:settlement (yes-sooner "2015-03-31"))

; Will 3-D printed light-sensitive retinal neuron cells be reported by March 31 2015?
(o 1111 (goes-to no 90 #:start "2015-02-01" #:stop "2015-04-07")
   #:settlement (yes-sooner "2015-03-31"))

; Will an eye-based biometric security system breach in the United States be reported by March 31, 2015? 
(o 1113 (goes-to no 92 #:start "2015-02-10" #:stop "2015-03-31")
   #:settlement (yes-sooner "2015-03-31"))

; Which team competing as of December 2014 will win the Google Lunar XPrize Grand Prize before December 31st, 2016?
(o 1120 '[1 1 1 1 1 1 60]
     #:settlement (list (sr 'now "2016-12-31")
                        (sr 'now "2016-12-31")
                        (sr 'now "2016-12-31")
                        (sr 'now "2016-12-31")
                        (sr 'now "2016-12-31")
                        (sr 'now "2016-12-31")
                        "2016-12-31"))

; When will a spacecraft land on the Moon and fulfill the requirements of the $20-million Google Lunar XPrize Grand Prize?
(o 1121 '[0 0 0 0 1 1 2 2 2 2 3 3 30]
   #:settlement (sr-bands '("2015-01-01" "2015-03-01"
                            "2015-06-01" "2015-09-01"
                            "2016-01-01" "2016-03-01"
                            "2016-06-01" "2016-09-01"
                            "2017-01-01" "2017-03-01"
                            "2017-06-01" "2017-09-01"
                            "2018-01-01")))

; Will a new drug resistant "Superbug" be responsible for a fatal infection in a U.S. hospital before March 31, 2015?
(o 1128 (goes-to no 85 #:start "2015-01-10" #:stop "2015-04-02")
   #:settlement (yes-sooner "2015-03-31"))

; Will a named storm form in the Atlantic Ocean between January 1st and March 31, 2015?
(o 1133 (goes-to no 90 #:start "2015-01-10" #:stop "2015-04-02")
   weak
   #:settlement (yes-sooner "2015-03-31"))

; Will a demonstration of aneutronic fusion to directly create electricity be announced in 2015?
(o 1134 (goes-to no 85 #:start "2015-01-10" #:stop "2015-12-31")
   weak
   #:settlement (yes-sooner "2015-12-31"))

; Will an unmanned aerial vehicle collide with a commercial airliner over U.S. airspace before March 31, 2015?
(o 1136 (goes-to no 97 #:start "2015-02-01" #:stop "2015-03-31")
   #:settlement (yes-sooner "2015-03-31"))

; Will a power outage occur in any part of the United States as a result of a cyber attack on a U.S. electric grid by March 31, 2015?
(o 1137 (goes-to no 85 #:start "2015-01-10" #:stop "2015-03-31")
   #:settlement (yes-sooner "2015-03-31"))

; How many Apple Watches will be sold during Apple's fiscal year ending in autumn 2015?
(o 1138 (linear-scale 22.5 #:lo 0.0 #:hi 200.0)
   #:settlement (yes-sooner "2015-12-31" #:frac 0.4))

; Will any OPEC country suspend or terminate it's membership before April 1st, 2015?
(o 1139 (goes-to no 95 #:start "2015-02-01" #:stop "2015-05-15")
   weak
   #:settlement (yes-sooner "2015-03-31"))

; Will D, the programming language, be among the ten most Googled software technologies in the US in any month during 2015?
(o 1144 (goes-to no 80 #:start "2015-01-01" #:stop "2016-12-31")
   #:settlement (yes-sooner "2015-12-31"))

; Will Swift, the programming language, be among the twenty most Googled software technologies in the US in any month in 2015?
(o 1145 (goes-to no 80 #:start "2015-02-01" #:stop "2016-02-01"))

; Will NASA spacecraft Dawn confirm the existence of an ocean of liquid water on Ceres, the dwarf planet in the asteroid belt?
(o 1148 (yes 5))

; Will an announcement of the discovery of an exoplanet using the optical vortex coronagraph method occur in 2015?
(o 1150 (goes-to no 95 #:start "2015-03-15" #:stop "2015-12-31"))

; What will be the peak percentage of users accessing Google via IPv6 at the end of July 2015?
(o 1152 (linear-scale 8.0 #:lo 6.5 #:hi 10.0) 0.2)

; Will teixobactin, a candidate antibiotic identified using Northeastern University's IChip device, enter a US human clinical trial before the end of 2017?
(o 1153 (goes-to no 60 #:start "2015-02-01" #:stop "2017-12-31"))

; Will a study of a functioning 3D-printed quantum dot LED contact lens be published by the end of 2016?
(o 1154 (goes-to no 55 #:start "2015-03-01" #:stop "2017-01-31"))

; Will WiGig technology be sold as a component of a commercially available smartphone in the U.S. by March 31, 2015?
(o 1155 (goes-to no 99 #:start "2015-01-20" #:stop "2015-03-01"))

; Will an unmanned aerial vehicle designed to deliver commercial packages be tested outdoors in a residential area in the continental U.S. by March 31st, 2015?
(o 1156 (goes-to no 85 #:start "2015-01-20" #:stop "2015-04-03"))

; Will a magnitude 4.0 earthquake, or greater, hit Irving, Texas before March 31, 2015?
(o 1159 (goes-to no 92.5 #:start "2015-03-01" #:stop "2015-04-01"))

; Will Mauna Loa, one of the five volcanoes that make up the island of Hawaii, erupt before March 31, 2015?
(o 1161 (yes 3))

; Will a 3D printed car be available for public purchase before March 31, 2015?
(o 1163 (goes-to no 97 #:start "2015-01-20" #:stop "2015-04-03"))

; When will the first externally introduced automobile computer virus be reported?
(o 1165 '[1 3 9 27] strong)

; When will the Isolation Chip (iChip), which was instrumental in discovering the new antibiotic Teixobactin, result in the discovery of another new antibiotic?
(o 1166 '[5 6 7 8])

; Will Xiaomi smartphones, widely popular in the Far East, have products launched in more than 10 countries by March 31, 2015?
(o 1167 (goes-to no 50 #:start "2015-02-01" #:stop "2015-05-01")
   weak)

; Will India have an operating thorium-fueled nuclear power plant by the end of 2016?
(o 1168 (goes-to no 60 #:start "2015-02-01" #:stop "2017-01-01"))

; Will Manhattan's municipal parking system launch near field communication (NFC) enabled pay-by-phone parking by June 30, 2015?
(o 1169 (goes-to no 66 #:start "2015-02-01" #:stop "2016-01-01"))

; Will a backscatter wireless device that can send at least 5Mb per second signals across more than 20 feet be reported by the end of 2015?
(o 1170 (yes 40) weak)

; Will the Hubble Telescope observe water vapor plumes on Europa for a second time before May 1, 2015?
(o 1171 (goes-to no 66 #:start "2015-01-25" #:stop "2015-04-30")
   #:settlement (yes-sooner "2015-05-01"))

; In 2015, how many weather and climate disaster events with losses exceeding $1 billion will occur in the United States?
(o 1172 (linear-scale 8.25 #:lo 0.0 #:hi 20.0)
   #:settlement (yes-sooner "2016-01-01" #:frac 0.75))

; Will a scientific paper from an independent institution detailing further animal testing of the antibiotic Teixobactin be published before the end of 2015?
(o 1174 (goes-to no 60 #:start "2015-03-15" #:stop "2016-03-15"))

; What will retail electricity sales be for the US in 2015?
(o 1178 (linear-scale 4850 #:lo 4600 #:hi 5200))

; Will researchers demonstrate a temporary tattoo containing an iontophoretic-biosensing platform to measure diabetic’s glucose levels that provides continuous monitoring for more than 8 hours?
(o 1181 (yes 40))

; When will a smartphone-based stroke detection system be available for consumer purchase?
(o 1182 '[1 2 3 7])

; When will one of the five major US broadcast networks first broadcast 4KTV over-the-air?
(o 1186 '[1 3 20])

; Will at least one death caused by measles in the US be reported in 2015?
(o 1189 (goes-to no 80 #:start "2015-02-27" #:stop "2015-12-31"))

; Will new investment in renewable energy in Europe be higher in 2015 than it was in 2014?
(o 1195 (yes 45))

; Will an Android phone offer body movement-based security by the end of 2016?
(o 1196 (goes-to no 60 #:start "2015-03-01" #:stop "2016-12-31"))

; Will China's new investments in renewable energy exceed $100 billion in 2015?
(o 1197 (yes 20))

; Before January 1st, 2017, will one of the top 5 Fortune 500 companies adopt a 30-day notification standard to alert customers of a data breach?
(o 1199 (yes 25))

; Will a data breach, compromising more than 100 million records be announced by the infiltrated company in 2015?
(o 1201 (goes-to no 35 #:start "2015-02-27" #:stop "2016-06-15"))

; Will Microsoft have to release a patch outside of its normal "Patch Tuesday" regime four or more times in 2015?
(o 1202 (goes-to no 87 #:start "2015-02-27" #:stop "2015-12-15"))

; Will one of the top 5 insurance companies in California offer a liability insurance package for driverless vehicles cheaper than the equivalent package for a human driven car by an under 25 year-old male driver by July 1st, 2016?
(o 1203 (yes 10))

; Will Apple revert iOS encryption to default opt-in by January 31, 2016?
(o 1204 (goes-to no 80 #:start "2015-02-27" #:stop "2016-02-27"))

; Will a botnet (or other persistent control regime) running on at least 500,000 SOHO routers be acknowledged in 2015?
(o 1205 (goes-to no 85 #:start "2015-02-27" #:stop "2016-03-27"))

; Will malware signed by a key not previously acknowledged to have been stolen be reported in the news at least once in 2015?
(o 1206 (goes-to no 25 #:start "2015-02-27" #:stop "2016-02-27"))

; Will a penetration of the Nest thermostat be reported by December 1, 2015?
(o 1208 (goes-to no 45 #:start "2015-02-27" #:stop "2016-01-31"))

