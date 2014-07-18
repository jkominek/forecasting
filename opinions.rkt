#lang racket/base

(require (planet bzlib/date/plt)
         racket/set
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

(provide opinion-database load-opinion-database have-opinion? get-opinion get-all-opinions)

(struct opinion [for-id beliefs strength]
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

(provide (rename-out [wrapped-opinion-beliefs opinion-beliefs]
		     [wrapped-opinion-strength opinion-strength]))

(define (o id beliefs [strength 1.0])
  (hash-set! (opinion-database)
	     id
	     (make-opinion
	      id
	      (if (procedure? beliefs)
		  beliefs
		  (lambda () beliefs))
	      (if (procedure? strength)
		  strength
		  (lambda () strength)))))

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
	  initial
	  (let ([scale (/ (- (current-seconds) start)
			  (- stop start))])
	    (f (+ (* (- 100 initial) scale) initial)))))))

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

; Which of the following changes will be reported about "trends in extent of selected biomes, ecosystems, and habitats" in the fourth edition of the Global Biodiversity Outlook report?
(o 2 '[20 50 30 5])

; Which of the following changes will be reported about "The Marine Trophic Index" in the fourth edition of the Global Biodiversity Outlook report?
(o 3 '[20 50 30 5])

; Which of the following changes will be reported about "trends in abundance and distribution of selected species " in the fourth edition of the Global Biodiversity Outlook report?
(o 4 '[20 50 30 5])

; Which of the following changes will be reported about "Water quality of aquatic ecosystems" in the fourth edition of the Global Biodiversity Outlook report?
;(o 5 '[20 50 30 5])

; Which of the following will be found to contain graph non-isomorphism by 1 Jan 2018?
(o 8 '[1 3 9])

; Will the internet usage worldwide exceed 45% of the world's population before Jan 01, 2015?
(o 18 (yes 10))

; Will Australia reduce its greenhouse gas emissions unconditionally by 5% below their 2000 levels by January 01, 2020?
(o 21 (yes 44))

; When will the Unique Games Conjecture be proven?
(o 25 '[0.5 5 10 20 30])

; When will the existance of a strongly polynomial-time algorithm for linear programming be proven?
(o 36 '[0.5 5 10 20])

; Will a proof be published by 2050 showing Matrix Multiplication in O(n^{2+ε}) time, for every constant ε > 0?
;(o 57 (no 60))

; Will there be a 50%-effective malaria vaccine available for general use before 2015?
(o 84 (no 75))

; When will a method store qubit superposition states indefinitely room temp less than 50 percent data loss?
(o 123 '[0 0.5 0.6 10])

; When will research cell efficiency least one type of solar cell in NREL emerging photovoltaic cell larger than CdTe search solar cell efficiency?
(o 125 '[0 0 0 0.1 1 10])

; Will a NASA astronaut be transported to the International Space Station in a commerical spacecraft by DEC 1 2017?
(o 132 '[1 2])

; Who will win the DARPA Robotics Challenge Finals in 2014?
(o 140 '[1 1 1 1 1 1 1 1 0.5] weak)

; Will a paper describing Andrea Rossi's alleged LERN device by published in a journal before Jan 1 2015?
(o 144 (yes 1))

; Will Rosetta rendezvous with Comet 67P/Churyumov-Gerasimenko? 
(o 169 (yes 90))

; How many teams competing in the Nokia Sensing XChallenge will receive distinguished awards?
(o 180 '[5 10 8 6 4 2])

; Will the US National Security Agency build a gate model quantum computer before the end of 2017?
(o 437 (yes 50) weak)

; #548 "When will the first artificial internal organ created using AAM technology be successfully transplanted into a human?"
(o 548 '[0.1 1 2 3 4 5 50])

; When will operation of AAM systems be considered a unique trade skill requiring specific licensing as defined by the National Council of Examiners for Engineering and Surveying (NCEES)?
(o 549 '[1 2 3 4 5 6 7])

; A “smart city” runs important systems—transit, water, power, trash removal—entirely by computers. When will a city with a population of at least one million people adopt smart city technology?
(o 568 '[1 2 3 4 5 6 7 8 9 10 11 15])

; Will Microsoft Office for iPad become one app instead of three apps (Word, Excel and Powerpoint) by the end of 2014?
(o 580 (no 75))

; When will the first primate be cloned?
(o 582 '[0.4 1 1 1 5])

; Will there be a RFID malware attack by the end of 2014?
(o 583 (no 90))

; When will the first reported collision occur between an autonomous vehicle and a human driven vehicle on a public road? 
(o 638 '[5 4 3 2 1])

; Will Google acquire Twitch by the end of September 2014?
(o 674 (yes 60))

; Will researchers provide a practical demonstration of the newly discovered weakness in the discrete logarithm problem, used widely in cryptography, by December 31, 2014?
(o 702 (yes 4))

; When will reseachers announce the conversion of methane to methanol at room temperature through the use of metal-organic frameworks?
(o 722 '[1 2 3 4 5 6 7 8 9 9] weak)

; When Microsoft launches Skype Translator, the real-time language translation service for its video chat application Skype, will it be a free add-on?
(o 706 (yes 85))

; When will Skype provide voice and text language translation to its customers during calls? 
(o 694 '[1 10 11 5])

; Will NASA's Cold Atom Lab successfully achieve a temperature of 100 picokelvin? 
(o 322 (yes 45) weak)

; Will NASA detect a near Earth object (NEO) with an estimated Earth impact risk of greater than 0.081 by the end of 2014? 
(o 326 (yes 1))

; Will Nature retract one or more of the January 2014 papers by H. Obokata et al. describing stimulus-triggered acquisition of pluripotency (STAP)?
(o 395 (yes 70))

; Will scientists create a fully air-transmissible, mammalian-infectious strain of avian influenza in a laboratory setting by the end of 2014?
(o 656 '(1 2) 0.08)

; Will Jupiter's Great Red Spot shrink below 9000 miles in diameter before January 1 2016?
(o 684 (goes-to no 80 #:start "2014-05-01" #:stop "2015-12-01")
	 (varying-strength "2014-05-01" "2015-12-01"))

;stable transactinide
(o 678 '(1 1 1 1 1
	   1 1 1 1 1
	   1 1 1 0.01 0.1
	   0.01 0.01 0.01 0.01 0.01
	   0.01 0.01 0.01 0.001 26))

; Will Virgin Galactic begin commercial flights aboard SpaceShipTwo by the end of 2014?
(o 670 (no 80) (varying-strength "2014-06-01" "2014-11-01"))

; Will a wearable camera with speed of more than 60 frames per second at 4K resolution be commercially available by the end of 2015?
(o 400 (yes 60) (varying-strength "2014-06-01" "2015-10-01"))

; Will implantable sensors for monitoring cancer enter clinical trials by the end of 2015?
(o 411 (yes 33.3))

; Will a dishonest Bitcoin mining pool successfully execute a 51% attack by December 31 2015?
(o 342 (yes 10))

; Will the Japan-based over-the-top (OTT) messaging company Line Corporation complete an IPO before the end of 2014? 
(o 425 (yes 20))

; Will a cellphone that charges itself using self-charging power cells (SCPC) be demonstrated publicly before the end of 2015?
(o 427 (yes 49))

; Will DARPA's Mobile Hotspot program enter Phase 3 by the end of June 2015?
(o 681 (yes 45) weak)

; When will a transactinide isotope with a half-life longer than 29 hours be discovered?
(o 680 '(1 1.5 2 2.5 3) strong)

; How fast will the next 100 meter human sprinting world record be?
; 2015 world championship in athletics is in beijing, ending sept 6th 2015
; unlikely (impossible?) any record could be set in 2015 after that.
(o 666 '[0.1 0.11 0.12 0.15 1 5]
   (varying-strength "2014-09-01" "2015-09-06"))

; Will there be at least one female winner of a Fields Medal in 2014?
(o 462 (yes 20) 0.1)

; #466 "How many venues will be listed on Coinmap on December 31, 2014?"
(o 466 '[0 0 0 0 1 5 10 20])

; Will Google make end-to-end PGP encryption a core feature in Gmail by August 2014?
(o 585 (no 98))

; When will the Deque Conjecture be proven?
(o 26 '[1 10 10 10 20])

; How many female chess players will hold a rating of at least 2700 on JUL 01 2016?
(o 318 '[10 5 1 0.01 0.001])

; Will the Apple iWatch be commercially available by the end of September 2014?
(o 401 (no 90))

; Will Bluefin-21 locate the black box from Malaysian Airlines flight MH-370?
;(o 464 (no 99) strong)

; Will 23andMe offer health-related genetic reports for new customers by December 2014?
(o 659 (yes 33.333) weak)

; Will the International Sun-Earth Explorer 3 start to collect data and send it to Earth?
(o 644 (yes 62.5))

; Will NASA land a telepresence robot on Venus by 2020?
(o 653 (no 98) (* 2 strong))

; When will UHD programming be available from Netflix on optical disc?
(o 587 '[9 20 25 46])

; When will a metropolitain area of over 250000 people wirelessly network all traffic signals and sensors so that an AI system can optimize 2-dimensional traffic flow in real-time?
(o 636 '(1 10 9 8 7 6))

; Will a solar powered Apple laptop with photovoltaic cells located in the display module be commercially available before the end of 2018?
(o 330 (no 90) weak)

; Will there be a new digital currency based on a P2P network, whose real money supply will be valued at more than $1 billion in 2014?
(o 102 (yes 25))

; Will Apple integrate a sapphire crystal solar power screen in its new iPhone6 due to be released later in 2014?
(o 354 (no 95))

; Will a VC-funded Bitcoin business initiate a public IPO by December 31 2014?
(o 625 (yes 15))

; Space agencies for the United States, the European Union, the Russian Federation, and China have announced “plans for plans” for manned missions to Mars, with target landing dates in the 2030s. When will a human being actually set foot on the surface of Mars? 
(o 532 (linear-scale 2035 #:lo 2014 #:hi 2036))

; When will a single confirmed exploitation of the Heartbleed bug result in exposure of personally identifiable information (PII) of more than 1 million users?
(o 539 '[0.1 0.1 1 1 1 100])

; Will quantum key distribution be integrated into a prototype mobile device by 1 January 2016?
(o 562 (no 75) weak)

; When will the first U.S. state enact legislation allowing consumer operation of driverless cars?
(o 149 '[1 1.5 2 2.5 3])

; Will the Axion Dark Matter Experiment detect axions by July 2014?
(o 434 (yes 0))

; When will the first chess player achieve a rating of at least 2900?
(o 313 '[5 4 3 2 1])

; Will Paypal integrate Bitcoin payments by February 28 2015?
(o 341 (no 90))

; Will Google integrate Bitcoin payments into Google Wallet by February 28 2015?
(o 338 (no 90))

; Will either the United States or China be able to build an encryption cracking quantum computer before the end of 2018?
(o 436 (yes 15))

; Will the identity of Satoshi Nakamoto be identified by December 31 2014?
(o 418 (yes 9.5))

; Will additional evidence of waves in Titan's seas be observed in new images by the end of the Cassini mission? 
(o 456 (yes 40))

; Will there be a hard fork to the Bitcoin block chain by December 31 2014?
(o 459 (yes 20))

; Will an orbiting body system be discovered in space before the end of 2019 which could verify at least one of the 13 new three-body problem solution families recently discovered by two Belgrade physicists?
(o 442 (no 90) strong)

; Will quantum key distribution be integrated into a commercially available mobile device by 1 January 2017?
(o 534 (no 90) (varying-strength "2015-01-01" "2016-12-01"
				 #:initial-strength weak
				 #:final-strength (* 2 strong)))

; Since 2009, the record speed for a robot performing the industry-standard pick-and-place test has been 5 cycles/second. What will be the fastest robotic pick-and-place rate achieved by the end of 2014? 
(o 384 (map (lambda (x) (/ (expt 3 x)))
            '[0 1 2 3 4 5]))

; In 2014, will an Atlantic hurricane or tropical storm prompt the mayor of New York City to declare a state of emergency?
(o 386 (yes 7.5))

; When will the first mass-produced multicopter, for human flight, be offered for sale?
(o 182 '[1 3 9 18])

; Will a breathalyzer for measuring blood sugar be commercially available by the end of 2016?
(o 402 (yes 40) weak)

; Will a solar cell with efficiency of at least 50% be reported by the end of 2017?
(o 424 (no 66))

; Will project NA62 at the CERN in Geneva make a discovery of new unknown particles not predicted by the Standard Model of particle physics in 2014?
(o 183 (no 98) weak)

; What will the unemployment rate for ACS chemists be in 2014?
(o 348 '[1 2 1 0.5 0.5 0.1] weak)

; What kind of a computing device will be used to create the first 8-man chess endgame tablebase?
(o 312 '[1 100 1 1 10] (varying-strength "2014-01-01" "2020-01-01"
					#:initial-strength (/ weak 4)
					#:final-strength strong))

; When will the Clay Mathematics Institute announce a winner of the Millennium Prize for resolving the "P vs NP Problem"?
(o 27 '[1 2 3 4 200])

; Which of the following changes will be reported about "Status and trends of linguistic diversity and numbers of speakers of indigenous languages" in the fourth edition of the Global Biodiversity Outlook report?
(o 99 '[0.5 10 1 0.1])

; Will the NASA's Mars Atmosphere and Volatile Evolution (MAVEN) spacescraft launched on November 17 2013 enter the Mars orbit by the end of September 2014?
(o 143 (yes 90))

; Will the silver nanowire ink touch sensitive screens being developed by 3M and Cambrios be in commercially available smartphones by the end of 2015?
(o 363 (yes 33) weak)

; Will the 9-man chess endgame tablebase be created before JAN 01 2030?
(o 311 (yes 50) (varying-strength "2014-06-01" "2029-01-01"))

; Will the trade price of Whole Milk Powder fall below $3000 per tonne by December 31 2014?
(o 685 (yes 80) (* 0.1 weak))

; (3) id 421 "Will the unit price of Chinese solar PV modules fall below 50 cents per watt in the US by the end of 2015?"
(o 421 (no 52))

; Will planet Kepler 62f be found to have water on its surface?
(o 608 (yes (* 1/3 ; chance of water
	       1/3 ; chance they'll look well enough to tell
	       100)))

; Will a commercial building with perovskite solar cell windows be completed by the end of 2018? 
(o 329 (goes-to yes 40 #:start "2014-06-01" #:stop "2018-08-01")
   (varying-strength "2014-06-01" "2018-07-01"
		     #:initial-strength 0.8
		     #:final-strength 1.2))

; When will the first Hyperloop-like system begin public operations?
(o 181 '[1 2 3 4 5 45])

; "By the end of 2014 will there be at least three US states that tax e-cigarettes?"
(o 686 (yes 55) (varying-strength "2014-06-01" "2014-12-01"))

; At its first launch, will Apple's iWatch include a sensor to measure blood glucose level non-invasively?
(o 687 (no 92))

; Will Google's glucose monitoring "smart" contact lenses get FDA approval by the end of 2016?
(o 688 (no 49) (varying-strength "2014-07-01" "2015-07-01"))

; Will a foldable tablet computer made with inkjet-printed graphene be commercially available by the end of 2018? 
(o 453 (goes-to no 66.666 #:start "2014-06-15" #:stop "2017-10-01")
   (varying-strength "2014-08-01" "2017-11-01"))

; November 2014 Top 500 Winner
(let ([tianhe2-wins 0.8])
  ; On November 2014 what will be the cores per socket of the Top 500 winner? 
  (o 45 (primary-option 2 tianhe2-wins
                        '[4 15 20 18 5])
     )

  ; On November 2014 what will be the geographic region of the Top 500 winner? 
  (o 44 (primary-option 1 tianhe2-wins
                        '[3 2 1 1 0.1])
     )

  ; On November 2014 what will be the vendor of the Top 500 winner? 
  (o 43 (primary-option 4 tianhe2-wins
                        '[10 14 3 3 2])
     )

  ; On November 2014 what will be the processor generation of the Top 500 winner? 
  (o 46 (primary-option 2 tianhe2-wins
                        '[5 8 20 10 5])
     )
  )

; Will the Axion Dark Matter Experiment detect dark matter axions by the end of 2014?
(o 128 (no 97.5) weak)

; Will the twin prime conjecture be resolved by 2024?
(o 200 (yes 50))

; Will a 100 million digit prime number be found before 2016?
(o 201 (yes 50))

; Will a genetically engineered organism that can break down grass plant materials to yield directly useful biofuel products, such as alcohols and alkanes, be reported before the end of 2015? 
(o 206 (yes 33) weak)

; How many mobile wireless devices in the U.S. will the FCC report as connected to the internet for the end of 2013? 
(o 217 '[10 1 0.1 0.01 0.001])

; Will lab experiments conducted with Josephson junctions prove the existence of axions by the end of 2015?
(o 227 (no 90))

; How many countries will self-report to have met WHO International Health Regulations (2005) core capacity requirements by AUG 01 2014?
(o 287 '[0.1 0.1 1 10 30])

; Will China have at least 30 nuclear power reactors in operation by the start of 2015?
(o 173 (no 95))

; Will evidence of proton decay be reported in the scientific literature by the end of 2017?
(o 508 (no 95))

; Will the Large Underground Xenon dark matter experiment in Lead, SD detect the dark matter particles weakly interacting massive particles (WIMPs) or weakly interacting slim particles (WISPs) by the end of 2014?
(o 145 (no 95))

; (10) id 438 "Will GE and Sasol's new AbMBR technology, which cleans waste water while generating biogas for power production, be comercially available by the end of June 2015?"
(o 438 (yes 45) (varying-strength "2014-07-01" "2015-05-15"))

; On NOV 2014 what will be the (performance-weighted) gigaflops per watt of the Top 500?
(o 175 '[96 3 1 0 0] (* 2/3 strong))

; On NOV 2015 what will be the (performance-weighted) gigaflops per watt of the Top 500?
(o 176 '[95 4 1 0 0] (* 2/3 strong))

; On NOV 2014 what will be the (performance-weighted) gigaflops per core of the Top 500? (177)
(o 177 '[0 80 20 0 0] strong)

; On NOV 2015, what will be the (performance-weighted) gigaflops per core of the Top 500?
(o 178 '[0 50 50 0 0] strong)

; In the week of November 23rd-29th 2014 will Walmart be searched more frequently than Amazon in Google Search?
(o 649 (yes 90))

; when will a fully autonomous (self-driving) car made by a major auto maker drive more than 60 miles in the united states without input from the driver?
(o 696 '[2 3 4 3 2 1] weak)

; On November 2014 what will be the Top 500 performance share by geographic region?
(let* ([current-performance
        (list 124825909 78651505 35330764 17099688
              (+ 2898745 6736440 2502392 2329579 2472259 1031365))]
       [nov2014-performance
        (map * (list 1.1 1.11 1.08 1.08 1.05)
             current-performance)]
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
  (o 48 nov2014-performance)

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
       [nov2014-performance current-performance]
       [jun2015-performance nov2014-performance]
       [nov2015-performance
        (map * '[0.9 1 1 1.1 1.2]
             jun2015-performance)]
       [nov2017-performance
        (map * '[0.9 1 1 1.1 1.2]
             nov2015-performance)]
       )
  ; nov 2014
  (o 56 nov2014-performance)

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
(let* ([current-performance (list 87748559 49817954 42817488 10912241 82784343)]
       [nov2014-performance current-performance]
       [jun2015-performance nov2014-performance]
       [nov2015-performance jun2015-performance]
       [nov2017-performance 
        (map * '[1 1 1.1 0.9 1.1]
             nov2015-performance)]
       )
  ; nov 2014
  (o 47 nov2014-performance)

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
(o 105 (yes 5) weak)

; Which of the following 2016 presidential candidates will be the first to accept Bitcoin campaign contributions? 
(o 371 '[3 0.5 0.5 9 3 1 30] weak)

; Will the Mars Curiosity Rover discover organic matter on Mars-evidence that life exists or existed on the planet-by July 1 2015?
(o 377 (no 98.5))

; When will the Chinese National Space Administration land a man or woman on the moon?
(o 136 '[1 9 10 10])

; Will the Chinese National Space Administration retrieve at least 2kg of lunar rock/soil samples by January 1 2018? 
(o 135 (yes 15) (varying-strength "2014-06-15" "2017-10-01"))

; In the 2013 Annual Report from the National Poison Data System (NPDS) will analgesics be the number one substance class for exposures among adults in the US?
(o 226 (yes 63.1))

; Will Google announce development of an Android In-Car System at or before the Google I/O 2014 Conference?
(o 698 (yes 100) 0.05)

; Will the average temperature for the contiguous U.S. during July 2014 be above 75oF?
(o 373 (yes 12) weak)

; Will there be at least one major hurricane that makes US landfall in the 2014 hurricane season?
(o 374 (yes 30))

; How many near-Earth large asteroids will NASA detect in 2014?
(o 321 (map *
            '[0.002 0.712 .279 0.0 0.0] ; from simple recent-only model
            '[.123 0.871 .006 0.0 0.0]  ; from large long-term population-estimating model
            ))

; When will 99% of the top 1 million web domains in the world be immune to Heartbleed? (538)
;(o 538 '[0.000 0.000 0.01 0.02 0.03 0.95] 0.025)

; Will the number of exposures to carbon monoxide reported in the 2013 Annual Report from the National Poison Data System (NPDS) be lower than 10000?
(o 189 (yes 0.9))

; Will there be 15 or more named storms (including subtropical storms) in the Atlantic-Caribbean-Gulf of Mexico region during the 2014 hurricane season?
(o 410 (yes 26) weak)

; What will be the total U.S. cherry production during the 2013 marketing season, in thousands of tons? (119)
(o 119 '[0 0 0 1 200 1] weak)

; When will floating wind turbines be used in a commercial offshore wind turbine farm?
(o 712 '[1 2 3 4 5 6])

; Will more new hybrid electric vehicles (including plug-in hybrid electric vehicles) be sold in the U.S. in 2014 than were sold in the U.S. in 2013? 
(o 718 (yes 50))

; Will hybrid and battery electric cars account for more than 4% of total new car sales in the U.S. in 2014?
(o 719 (yes 20))

; When will a prickless glucose monitor be commercially available to consumers?
(o 709 '[6 7 7 8 7 4])

; Will a new clean & jerk world record be set before or during the 2016 Olympic Games?
(o 711 (yes 25))

; Will Huawei confirm allegations that "back doors" where installed on their computer hardware before being sold?
(o 623 (yes 10) 0.1)

; Will a solar-powered plane circumnavigate Earth before the end of 2015?
(o 629 (yes 33) weak)

; Will Google incorporate infrared vision into future Google Glass models by the end of 2017?
(o 535 (yes 49))

; In 2014 will the Arctic sea ice minimum day occur after September 15th?
(o 385 (yes 50) weak)

; Will obesity be among the top five health-related keyword searches in Google in any month in 2014?
(o 188 (yes 3) weak)

; When will a classical algorithm be authored to solve 3-SAT faster than exp((o n))?
(o 32 '[0 1 2 3 6] weak)

; Internet of things 20 billion connected devices at end of 2013. how many at end of 2014?
(o 566 '[1 10 11 10 8] weak)

; world produced 4.4 zettabytes in 2013. how much in 2014?
(o 620 '[0.2 2 9 10 6] weak)

; Will the Mars Curiosity Rover discover organic matter on Mars by September 1 2014?
(o 127 (no 99.999))

; which smallsat company will be the first to provide daily re-imaging of the earth?
(o 714 '[40 80 10 75 80 50])

; When the consumer version of Google Glass is released to the general public, what will its price be? 
(o 399 '[1 8 4])

; Doctors saved the life of a boy suffering from encephalitis when a sequencing test turned up DNA of a lethal bacteria in his cerebrospinal fluid. What type of pathogen will this test discover next in a critically-ill patient suffering from encephalitis? 
(o 720 '[4 6 2 3 1 12])

; Will a Tesla electric car with a base price under $35,000 be for sale in the U.S. before the end of 2016? 
(o 717 (yes 45))

; "When will free-form gestures be used as access passwords in commercial touchscreen mobile devices?"
(o 724 '[2 2 3 3])

; Which team will win the 2014 AUVSI RoboBoat Competition?
(o 645
   '[0 ; cedar
     0 ; dipo
     1 ; embry
     0 ; florida atlantic
     0 ; georg
     0 ; national
     0 ; old dom
     0 ; old dom met?
     0 ; ucf rob club
     0 ; unv flor
     0 ; ut arling
     0 ; hawaii
     0 ; u of m
     0 ; univ rhod
     0 ; villa
     0 ; virginia tech
     ]
   (* 10 strong))

; pluto's exact size
(o 723 '[1 1]) ; range from 2280km to 2400km

; free-form gesures be used as access passwords in commercial touchscreen mobile devices?
(o 724 '[10 10 10 5])

; high-precision measurement of antiproton's magnetic moment?
(o 725 '[10 11 12 15] weak)

; MH370 debris
(o 726 '[2 1 2] weak)

; MH370 search days
(o 727 (linear-scale 300 #:lo 0 #:hi 400) weak)

; MH370 search contract
(o 728 (linear-scale 90 #:lo 25 #:hi 275))

; When will the traversal conjecture be proven?
(o 29 (list (/ (- 1420095600 (current-seconds))
               (- 1420095600 1388559600))
            5 10 20 30))

; Worldwide Semiconductor Sales reached US$305.6 billion in 2013. What will be the total for 2014?
(o 379 '[1 2 3 4 15 5])

; Will cyber threats be more costly than physical threats to the U.S. national electric grid in 2014? 
(o 588 '[1 1] weak)

; Will a genetically engineered organism that can break down grass plant materials to yield directly useful biofuel products, such as alcohols and alkanes, be reported before the end of 2015? 
(o 206 (yes 20) weak)

; What will the hashrate of the Bitcoin blockchain be on September 30, 2014?
(o 325 '[0 0 0 1 99])

; When will a piloted solar plane fly around the world only on solar energy?
(o 721 '[1 2 2 2 2 2 2] 0.1)

; Will any government officially accept a digital currency for circulation in their country in 2014?
;(o 103 (yes 55))

; Which private company will be the first to shuttle an astronaut to the International Space Station? 
(o 133 '[1 1 30 1 33])

; Will the Indian Space Research Organization's Mangalyaan probe achieve orbital insertion around Mars before NASA's MAVEN spacecraft in September 2014?
(o 153 (yes 18))

; When will an article describing the observation of a Population III star be published in a journal indexed by Thomson Reuter’s Web of Science? 
(o 170 '[1 2 4 8 32])

; Which organization will be the first to observe a Population III star?
(o 171 '[1 1 1 1 1 1 6])

; How many teams will be awarded a full prize in the final round of the Qualcomm Tricorder X Prize competition?
(o 184 '[37.08 37.85 18.24 6.83])

; Will the clinical trial being carried out at Penn State University into selenium supplements for men at risk for prostate cancer show that the mineral has a protective effect?
(o 207 (yes 5))

; When will a supersonic aircraft be available again for commercial travel?
(o 320 '[21 8 7 6 5])

; When will the Bitcoin blockchain first register over 150,000 transactions in a single day?
(o 337 (cons 0 (for/list ([i (in-range 0 4)]) (expt 11 i))))

; When will the Rosetta's small robotic lander, Philae, land successfully on the surface of a comet?
(o 349 '[0.1 0.5 9.75 8.75 7.5 6.25 5 12])

; Will US wind energy consumption decrease in 2014? 
(o 359 (yes 10))

; Will the consumer version of Google Glass be available for purchase in the US before the end of September 2014?
(o 398 (yes 10))

; Will Facebook use drones perhaps high-altitude, solar-powered craft like those made by Titan Aerospace to provide internet access in places where it is not currently accessible, by the end of December 2017?
(o 531 (yes 10))

; Will the global gender gap in the number of internet users decline in 2014? 
(o 552 (yes 50))

; When will Google or Facebook debut internet connectivity to consumers using solar-powered drones?
(o 553 '[0.1 1 10 100])

; What percentage of Japan's electricity generation in 2014 will come from nuclear power? 
(o 554 '[10 22 1 .1 .01 .001])

; What percentage of Japan's electricity generation in 2015 will come from nuclear power? 
(o 555 '[.1 1 1 1 .1 .01])

; Will Google or Facebook first debut internet connectivity to consumers using solar-powered drones? 
(o 557 '[10 1 10])

; Will a VC-funded Bitcoin business declare bankruptcy by December 31, 2014?
(o 624 (no 95))

; How many threatened languages will the Ethnologue language catalog report in its 18th edition?
(o 663 '[1 5 30 50 30])

; When will the Bailiwick of Jersey have over 50 companies that accept Bitcoin as a form of payment?
(o 729 '[4 5 5 6])

; When will the Australian Government award the contract for RFT for provision for services relating to the search for MH370?
(o 730
   (map * 
        '[0  7   5   7   7   6   6   14   ]   ; business days in period
        '[ 0 .7 .7 .62 .51 .40 .29  .18])  ; relative likelihood of period
   weak)

; At its initial launch, will the range of Tesla's Gen 3 electric car be greater than 150 miles? 
(o 731 (yes 90))

; In June 2014, Amazon unveiled the "Fire", the world's first 3-D smartphone. Which of these companies will be next to release a 3-D smartphone? 
(o 732 '[100 210 190])

; Will a Phase 1 clinical trial start recruiting participants before the end of 2014 for a study of the treatment of autism using suramin, a century-old treatment for sleeping sickness?
(o 733 (yes 50) weak)

; Will physicists find any difference in the magnetic moments of the antiproton and the proton when they achieve a direct high-precision measurement of the antiproton?
(o 734 (yes 25))

; When will the Australian contracted search for MH 370 begin?
(o 735 '[0.5 12 12 11 10 9 8])

; Engineered water nanostructures have been shown to inactivate dangerous pathogens at eight times the natural rate without toxic residue. How much faster than the natural rate will this technology inactivate surface bacteria according to the next report?
(o 736 '[10 9 8 7 6])

; When will a proof be published showing L=RL?
(o 31 '[0.5 5 10 20 5])

; Will the Hubble telescope identify a suitable Kuiper belt object for NASA's New Horizon's space probe to investigate after its flyby of Pluto before October 1, 2014?
(o 737 (yes 45))

; SciCast Ad Engagement: Over 3,800 people registered using SciCast's referral id 'bannerad' or 'Community' around 21-JUN-2014. What proportion will trade between 30 & 37 days after their account creation?
(o 739 (linear-scale 0.00666 #:lo 0.0 #:hi 0.05))

; Will Solar Roadways be installed on any public roadway in the U.S. for public use by 2020?
(o 742 (yes 20))

; Will researchers observe neutrinoless double-Î² decay to verify the Majorana nature of the neutrino by January 1, 2017? 
(o 746 (yes 25))

; A recent analysis of the nitrogen in Titan's atmosphere found the moon's atmospheric nitrogen originated in conditions similar to ancient comets from the Oort cloud. Will New Horizon's analysis of Pluto's atmospheric nitrogen find similar conditions?
(o 745 (yes 66))

; A NASA research team is currently conducting a small, low-budget experiment attempting to create a warp in spacetime. Will NASA's White?Juday warp-field interferometer experiment create a a microscopic instance of a warping of spacetime before 2020?
(o 744 (yes 15))

; In which of the following branches will the 2014 Nobel Prize in Physics be awarded?
(o 74 '[33 33 15 15 3])

; In which branch will the 2014 Nobel Prize in Chemistry be awarded?
(o 75 '[5 5 33 33 5 5])

; In which of the following subfields will the 2014 Nobel Prize in Physics be awarded?

(o 107
    '[
      ; cosmology
      5   2   1   1   2
      ; space physics
      1   1   5   3   2
      ; nano
      5   3   1   2   1
      ; chemical
      2   4   2   1   1
      ; nuke astro
      1   1   1  10 ])

; In which of the following sub-branches will the 2014 Nobel Prize in Chemistry be awarded?
(o 108 '[
         ; electro
         1 1 1 1 1
         ; quantum
         1 2 1 2 2
         ; phys org
         1 1 1 1 1
         ; inorg tech
         1 1 5 1 1
         ; enzym
         1 1 4 1 5
         ])

; Will there be a US federal government agency that imposes regulation on Bitcoin in 2014?
(o 225 (yes 15))

; When will a proof be published showing graph isomorphism is solvable in classical polynomial time?
(o 24 '[0.5 5 10 20 30] weak)

; Will the confirmed conversion efficiency for a multijunction, or tandem, solar cell (with any number of junctions) reach or exceed 46% by the end of 2014? 
(o 129 (yes 20))

; What will be the average rating of the top 10 chess players in the world on JAN 01 2016? 
(o 314 '[1 100 10])

; Will the U.S. be the world leader in total oil production in 2015?
(o 484 (yes 80))

; If the icy surface of Pluto's giant moon Charon is cracked, analysis of the fractures could reveal if its interior was warm, perhaps warm enough to have maintained a subterranean ocean of liquid water. Will researchers observe cracks in Charon's surface?
(o 740 (yes 40))

(let* ([today (seconds->date (current-seconds))]
       [up-to-date-day (- (date-day today) 3/2)]
       [season-start -5]
       [days-remaining (- 31 up-to-date-day)]
       [days-so-far (- up-to-date-day season-start)]
       
       [last-report-plain (set 'ca 'az 'co 'tx 'ok 'la 'mo 'tn 'al 'ga 'ia 'sd 'mn 'wi)]
       [last-report-neuro (set 'ca 'az 'mo 'tn 'ms 'al 'ga)]
       [since-plain (set 'ne)]
       [since-neuro (set)]
       
       [neuro-states (set-union last-report-neuro since-neuro)]
       [any-states
        (set-union last-report-plain since-plain neuro-states)]
       
       [neuro-rate (* 5/10 (/ (set-count neuro-states) days-so-far))]
       [any-rate (* 5/10 (/ (set-count any-states) days-so-far))]

       [neuro-predicted (+ (set-count neuro-states)
                           (* neuro-rate days-remaining))]
       [any-predicted (+ (set-count any-states)
                         (* any-rate days-remaining))])
  (printf "neuro west nile ~a~n" (exact->inexact neuro-predicted))
  ; How many states will report at least one case of a West Niles virus human neuroinvasive disease by 1 August 2014?
  (o 747 (linear-scale neuro-predicted #:lo 1 #:hi 50) weak)

  (printf "human west nile ~a~n" (exact->inexact any-predicted))
  ; How many states will report at least one case of a human West Nile infection to the CDC by 1 August 2014?
  (o 738 (linear-scale any-predicted #:lo 1 #:hi 50))
  )

; #748 "What will be the peak percentage of users accessing Google via IPv6 at the end of January 2015?"
(o 748 (linear-scale 5.273 #:lo 4.5 #:hi 6.0))

; When will photonic waveguide sensors being developed by Corning Inc. and Polytechnique Montreal be used in a commercially available smartphone?
(o 749 '[0.1 1 2 3 6])

; Will there be a locally transmitted case of Chikungunya virus in the contiguous U.S. by September 1, 2014?
(o 750 (yes 100))

; What programming language will be used by the 2015 winner of the ICFP Programming Contest?
(o 754 '[5 8 1 1 4 10 8 4 5 8])

; #755 "A 500-meter loop of elevated networked sky cars is planned to be built in Tel Aviv. When will it carry its first passenger?"
(o 755 '[0 10 15 20 20 15 10 10 10 20])

; How tall will Kingdom Tower in Saudi Arabia be when completed?
(o 756 (linear-scale 1010 #:lo 999.0 #:hi 1060.0))

; Will a smartphone that incorporates a temperature sensor in the glass be publically available by Jan 1 2017?
(o 757 (yes 15))

; When will a handheld, flexible nanocellulose video screen be comercially available for sale?
(o 759 '[1 2 2 3 3 4 9])

; Which of the following battery types mentioned in the C&EN July 2014 issue will be the first to be used in an electric car sold by 2020?
(o 760 '[4 2 4 2 1]) 

; When will the pharmaceutical industry produce a new drug to aid the deaf, by targeting the inner ear?
(o 762 '[1.5 2 2 3 3 2])

; Will NIST create uniform standards across all OSAC defined forensic disciplines by 2019?
;(o 763 (yes 25))
