#lang racket/base

(require (planet bzlib/date/plt)
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
(o 2 '[20 50 30 5] weak)

; Which of the following changes will be reported about "The Marine Trophic Index" in the fourth edition of the Global Biodiversity Outlook report?
(o 3 '[20 50 30 5] weak)

; Which of the following changes will be reported about "trends in abundance and distribution of selected species " in the fourth edition of the Global Biodiversity Outlook report?
(o 4 '[20 50 30 5] weak)

; Which of the following changes will be reported about "Water quality of aquatic ecosystems" in the fourth edition of the Global Biodiversity Outlook report?
;(o 5 '[20 50 30 5] weak)

; Will Australia reduce its greenhouse gas emissions unconditionally by 5% below their 2000 levels by January 01, 2020?
(o 21 (yes 44))

; Will a proof be published by 2050 showing Matrix Multiplication in O(n^{2+ε}) time, for every constant ε > 0?
;(o 57 (no 60))

; Will there be a 50%-effective malaria vaccine available for general use before 2015?
(o 84 (no 75) weak)

; Who will win the DARPA Robotics Challenge Finals in 2014?
(o 140 '[1 1 1 1 1 1 1 1 0.5] weak)

; Will Rosetta rendezvous with Comet 67P/Churyumov-Gerasimenko? 
(o 169 (yes 90))

; Will the US National Security Agency build a gate model quantum computer before the end of 2017?
(o 437 (yes 50) weak)

; When will operation of AAM systems be considered a unique trade skill requiring specific licensing as defined by the National Council of Examiners for Engineering and Surveying (NCEES)?
(o 549 '[1 2 3 4 5 6 7])

; A “smart city” runs important systems—transit, water, power, trash removal—entirely by computers. When will a city with a population of at least one million people adopt smart city technology?
(o 568 '[1 2 3 4 5 6 7 8 9 10 11 12])

; Will Microsoft Office for iPad become one app instead of three apps (Word, Excel and Powerpoint) by the end of 2014?
(o 580 (no 75) weak)

; Will there be a RFID malware attack by the end of 2014?
(o 583 (no 90) weak)

; When will the first reported collision occur between an autonomous vehicle and a human driven vehicle on a public road? 
(o 638 '[5 4 3 2 1])

; Will researchers provide a practical demonstration of the newly discovered weakness in the discrete logarithm problem, used widely in cryptography, by December 31, 2014?
(o 702 (yes 4))

; When will reseachers announce the conversion of methane to methanol at room temperature through the use of metal-organic frameworks?
(o 722 '[1 2 3 4 5 6 7 8 9 9] weak)

; When Microsoft launches Skype Translator, the real-time language translation service for its video chat application Skype, will it be a free add-on?
(o 706 (yes 85))

; When will the IPv6 traffic measured by Google surpass 4%?
(o 344 '(0 0 99.999 0.0001 0 0))

; what happened to mh370
(o 365 '(0 0 0 0 0 100) (* 5 strong))

; Will Nature retract one or more of the January 2014 papers by H. Obokata et al. describing stimulus-triggered acquisition of pluripotency (STAP)?
(o 395 (yes 70) weak)

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
(o 670 (no 60) (varying-strength "2014-06-01" "2014-11-01"))

; Will a wearable camera with speed of more than 60 frames per second at 4K resolution be commercially available by the end of 2015?
(o 400 (yes 60) (varying-strength "2014-06-01" "2015-10-01"))

; Will implantable sensors for monitoring cancer enter clinical trials by the end of 2015?
(o 411 (yes 33.3))

; Will a dishonest Bitcoin mining pool successfully execute a 51% attack by December 31 2015?
(o 342 (yes 15) (varying-strength "2014-05-01" "2015-12-01"
				  #:initial-strength weak
				  #:final-strength strong))

; Will a cellphone that charges itself using self-charging power cells (SCPC) be demonstrated publicly before the end of 2015?
(o 427 (yes 49) weak)

; Will DARPA's Mobile Hotspot program enter Phase 3 by the end of June 2015?
(o 681 (yes 45) weak)

; When will a transactinide isotope with a half-life longer than 29 hours be discovered?
(o 680 '(1 1.5 2 2.5 3) strong)

; How fast will the next 100 meter human sprinting world record be?
; 2015 world championship in athletics is in beijing, ending sept 6th 2015
; unlikely (impossible?) any record could be set in 2015 after that.
(o 666 '[0.1 0.11 0.12 0.15 1 5]
   (varying-strength "2014-09-01" "2015-09-06"))

; Will Google announce development of a smartwatch at or before the Google I/O 2014 Conference?
;(o 672 (yes 1) weak)

; Will there be at least one female winner of a Fields Medal in 2014?
(o 462 (yes 20) 0.1)

; Will Google make end-to-end PGP encryption a core feature in Gmail by August 2014?
(o 585 (no 98) strong)

; When will the Deque Conjecture be proven?
(o 26 '[1 10 10 10 20] weak)

; How many female chess players will hold a rating of at least 2700 on JUL 01 2016?
(o 318 '[10 5 1 0.01 0.001] weak)

; Will Apple announce development of an iWatch at or before the 2014 Worldwide Developer's Conference (WWDC)?
;(o 444 (no 99) strong)

; Will the Apple iWatch be commercially available by the end of September 2014?
(o 401 (no 90) (varying-strength "2014-06-05" "2014-08-20"
				 #:initial-strength weak
				 #:final-strength strong))

; Will Bluefin-21 locate the black box from Malaysian Airlines flight MH-370?
;(o 464 (no 99) strong)

; Will 23andMe offer health-related genetic reports for new customers by December 2014?
(o 659 (yes 33.333) weak)

; Will the International Sun-Earth Explorer 3 be successfully commanded to fire its thrusters by researchers?
(o 643 (yes 90))

; Will the International Sun-Earth Explorer 3 start to collect data and send it to Earth?
(o 644 (yes 64) weak)

; Will NASA land a telepresence robot on Venus by 2020?
(o 653 (no 98) (* 2 strong))

; When will UHD programming be available from Netflix on optical disc?
(o 587 '[9 20 25 46])

; When will a metropolitain area of over 250000 people wirelessly network all traffic signals and sensors so that an AI system can optimize 2-dimensional traffic flow in real-time?
(o 636 '(1 10 9 8 7 6))

; Will a solar powered Apple laptop with photovoltaic cells located in the display module be commercially available before the end of 2018?
(o 330 (no 90) weak)

; Will there be a new digital currency based on a P2P network, whose real money supply will be valued at more than $1 billion in 2014?
(o 102 (yes 25) weak)

; Will Apple integrate a sapphire crystal solar power screen in its new iPhone6 due to be released later in 2014?
(o 354 (no 95))

; Will a VC-funded Bitcoin business initiate a public IPO by December 31 2014?
(o 625 (no 85) weak)

; When will a single confirmed exploitation of the Heartbleed bug result in exposure of personally identifiable information (PII) of more than 1 million users?
(o 539 '[0.1 0.1 1 1 1 100])

; Will quantum key distribution be integrated into a prototype mobile device by 1 January 2016?
(o 562 (no 75) weak)

; When will the first U.S. state enact legislation allowing consumer operation of driverless cars?
(o 149 '[1 1.5 2 2.5 3] weak)

; Will the Axion Dark Matter Experiment detect axions by July 2014?
(o 434 (yes 0))

; When will the first chess player achieve a rating of at least 2900?
(o 313 '[3 3 2 2 1])

; Will Paypal integrate Bitcoin payments by February 28 2015?
(o 341 (no 90) weak)

; Will Google integrate Bitcoin payments into Google Wallet by February 28 2015?
(o 338 (no 90) weak)

; Will either the United States or China be able to build an encryption cracking quantum computer before the end of 2018?
(o 436 (yes 15))

; Will the identity of Satoshi Nakamoto be identified by December 31 2014?
(o 418 (yes 10))

; Will there be a hard fork to the Bitcoin block chain by December 31 2014?
(o 459 (yes 20))

; Will an orbiting body system be discovered in space before the end of 2019 which could verify at least one of the 13 new three-body problem solution families recently discovered by two Belgrade physicists?
(o 442 (no 90) strong)

; Will quantum key distribution be integrated into a commercially available mobile device by 1 January 2017?
(o 534 (no 90) (varying-strength "2015-01-01" "2016-12-01"
				 #:initial-strength weak
				 #:final-strength (* 2 strong)))

; Where will the Malaysia Airlines Flight MH370 be found?
(o 364 '[1 1 1 1 1 1 1000] (* 5 strong))

; When will the first mass-produced multicopter, for human flight, be offered for sale?
(o 182 '[1 2 3 5])

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
(o 27 '[1 2 3 4 50] (varying-strength "2045-01-01" "2050-01-01"
				     #:initial-strength weak
				     #:final-strength strong))

; Which of the following changes will be reported about "Status and trends of linguistic diversity and numbers of speakers of indigenous languages" in the fourth edition of the Global Biodiversity Outlook report?
(o 99 '[0.5 10 1 0.1] (/ weak 5))

; Will the NASA's Mars Atmosphere and Volatile Evolution (MAVEN) spacescraft launched on November 17 2013 enter the Mars orbit by the end of September 2014?
(o 143 (yes 90) (* weak 1/3))

; Will the silver nanowire ink touch sensitive screens being developed by 3M and Cambrios be in commercially available smartphones by the end of 2015?
(o 363 (yes 33) weak)

; Will the 9-man chess endgame tablebase be created before JAN 01 2030?
(o 311 (yes 50) (varying-strength "2014-06-01" "2029-01-01"))

; Will the trade price of Whole Milk Powder fall below $3000 per tonne by December 31 2014?
(o 685 (yes 15) weak)

; (3) id 421 "Will the unit price of Chinese solar PV modules fall below 50 cents per watt in the US by the end of 2015?"
(o 421 (no 51) (varying-strength "2014-06-10" "2014-12-31"
				 #:initial-strength strong
				 #:final-strength weak))

; Will planet Kepler 62f be found to have water on its surface?
(o 608 (yes (* 1/3 ; chance of water
	       1/3 ; chance they'll look well enough to tell
	       100))
   (/ weak 2))

; Will a commercial building with perovskite solar cell windows be completed by the end of 2018? 
(o 329 (goes-to yes 40 #:start "2014-06-01" #:stop "2018-08-01")
   (varying-strength "2014-06-01" "2018-07-01"
		     #:initial-strength 0.8
		     #:final-strength 1.2))

; When will the first Hyperloop-like system begin public operations?
(o 181 '[1 2 3 4 5 45] weak)

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
     strong)

  ; On November 2014 what will be the geographic region of the Top 500 winner? 
  (o 44 (primary-option 1 tianhe2-wins
                        '[3 2 1 1 0.1])
     strong)

  ; On November 2014 what will be the vendor of the Top 500 winner? 
  (o 43 (primary-option 4 tianhe2-wins
                        '[10 14 3 3 2])
     strong)

  ; On November 2014 what will be the processor generation of the Top 500 winner? 
  (o 46 (primary-option 2 tianhe2-wins
                        '[5 8 20 10 5])
     strong)
  )

; Will the Axion Dark Matter Experiment detect dark matter axions by the end of 2014?
(o 128 (no 95))

; Will lab experiments conducted with Josephson junctions prove the existence of axions by the end of 2015?
(o 227 (no 90) weak)

; Will China have at least 30 nuclear power reactors in operation by the start of 2015?
(o 173 (no 95) weak)

; Will evidence of proton decay be reported in the scientific literature by the end of 2017?
(o 508 (no 95) weak)

; Will the Large Underground Xenon dark matter experiment in Lead, SD detect the dark matter particles weakly interacting massive particles (WIMPs) or weakly interacting slim particles (WISPs) by the end of 2014?
(o 145 (no 95) weak)

; (10) id 438 "Will GE and Sasol's new AbMBR technology, which cleans waste water while generating biogas for power production, be comercially available by the end of June 2015?"
(o 438 (yes 45) (varying-strength "2014-07-01" "2015-05-15"))

; On NOV 2014 what will be the (performance-weighted) gigaflops per watt of the Top 500?
(o 175 '[92 3 1 0 0] (* 2/3 strong))

; On NOV 2015 what will be the (performance-weighted) gigaflops per watt of the Top 500?
(o 176 '[90 3 1 0 0] (* 2/3 strong))

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
             jun2015-performance)])
  ; nov 2014
  (o 48 nov2014-performance)

  ; jun 2015
  ; ???

  ; nov 2015
  (o 58 nov2015-performance)
  )

; On November 2014 what will be the Top 500 performance share by cores per socket?
(let* ([current-performance
        (list (+ 4451142 21457755) 99215856 (+ 23363299 53226459) 72366076 100)]
       [nov2014-performance current-performance]
       [jun2015-performance nov2014-performance]
       [nov2015-performance jun2015-performance])
  ; nov 2014
  (o 56 nov2014-performance)

  ; jun 2015
  ; ???

  ; nov 2015
  (o 49 nov2015-performance))

; On November 2014 what will be the Top 500 performance share by vendor?
(let* ([current-performance (list 87748559 49817954 42817488 10912241 82784343)]
       [nov2014-performance current-performance]
       [jun2015-performance nov2014-performance]
       [nov2015-performance jun2015-performance])
  ; nov 2014
  (o 47 nov2014-performance)

  ; jun 2015
  ; ???

  ; nov 2015
  (o 57 nov2015-performance)
  )

; Will Amazon deliver its first package using an unmanned aerial vehicle by DEC 31 2017?
(o 105 (yes 5) weak)

; Which of the following 2016 presidential candidates will be the first to accept Bitcoin campaign contributions? 
(o 371 '[3 0.5 1 9 3 1 31] weak)

; Will the Mars Curiosity Rover discover organic matter on Mars-evidence that life exists or existed on the planet-by July 1 2015?
(o 377 (no 98.5))

; When will the Chinese National Space Administration land a man or woman on the moon?
(o 136 '[1 9 10 10])

; Will the Chinese National Space Administration retrieve at least 2kg of lunar rock/soil samples by January 1 2018? 
(o 135 (yes 15) (varying-strength "2014-06-15" "2017-10-01"))

; On NOV 2014 what will be the (performance-weighted) gigaflops per core of the Top 500? (177)
(o 177 '[0 80 20 0 0] strong)

; On NOV 2015, what will be the (performance-weighted) gigaflops per core of the Top 500?
(o 178 '[0 50 50 0 0] strong)

; In the 2013 Annual Report from the National Poison Data System (NPDS) will analgesics be the number one substance class for exposures among adults in the US?
(o 226 (yes 63.1) weak)

; Will Google announce development of an Android Silver class of devices at or before the Google I/O 2014 Conference?
;(o 700 (yes 0.1) 0.05)

; Will Google announce development of an Android In-Car System at or before the Google I/O 2014 Conference?
(o 698 (yes 100) 0.05)

; android tv?
;(o 699 (yes 100))

; Will the average temperature for the contiguous U.S. during July 2014 be above 75oF?
(o 373 (yes 12) weak)

; How many near-Earth large asteroids will NASA detect in 2014?
(o 321 `[0.032 0.909 ,(- 0.058 0.02) 0 0])

; When will 99% of the top 1 million web domains in the world be immune to Heartbleed? (538)
;(o 538 '[0.000 0.000 0.01 0.02 0.03 0.95] 0.025)

; Will Stanford's Folding@Home distributed computing project exceed 30 PetaFLOPS on July 1st 2014?
(o 563 (yes 0) strong)

; Will the number of exposures to carbon monoxide reported in the 2013 Annual Report from the National Poison Data System (NPDS) be lower than 10000?
(o 189 (yes 0.9))

; Will there be 15 or more named storms (including subtropical storms) in the Atlantic-Caribbean-Gulf of Mexico region during the 2014 hurricane season?
(o 410 (yes 26) weak)

; What will be the total U.S. cherry production during the 2013 marketing season, in thousands of tons? (119)
(o 119 '[1 2 3 4 20 5] weak)

; When will floating wind turbines be used in a commercial offshore wind turbine farm?
(o 712 '[1 2 3 4 5 6])

; When will a prickless glucose monitor be commercially available to consumers?
(o 709 '[6 7 7 8 7 4])

; Will a new clean & jerk world record be set before or during the 2016 Olympic Games?
(o 711 (yes 25) weak)

; Will Huawei confirm allegations that "back doors" where installed on their computer hardware before being sold?
(o 623 (yes 10) 0.1)

; Will a solar-powered plane circumnavigate Earth before the end of 2015?
(o 629 (yes 33) weak)

; Will Google incorporate infrared vision into future Google Glass models by the end of 2017?
(o 535 (yes 49) weak)

; In 2014 will the Arctic sea ice minimum day occur after September 15th?
(o 385 (yes 50) weak)

; Will a hurricane or tropical storm form in the Atlantic-Caribbean-Gulf of Mexico region in June 2014?
(o 408 (yes 0.5) strong)

; Will the USPTO issue more than 750 nanotechnology class 977 patents in the first six months of 2014 (1/1/14-6/30/14)?
(o 185 (yes 0.75) weak)

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
(o 720 '[4 6 2 3 1 12] weak)

; Will a Tesla electric car with a base price under $35,000 be for sale in the U.S. before the end of 2016? 
(o 717 (yes 45))

; "When will free-form gestures be used as access passwords in commercial touchscreen mobile devices?"
(o 724 '[2 2 3 3])

; Which team will win the 2014 AUVSI RoboBoat Competition?
(o 645 (build-list 16 (lambda x 1)) 0.001)

; pluto's exact size
(o 723 '[1 1] weak) ; range from 2280km to 2400km

; free-form gesures be used as access passwords in commercial touchscreen mobile devices?
(o 724 '[10 10 10 5])

; high-precision measurement of antiproton's magnetic moment?
(o 725 '[10 11 12 15] weak)

; MH370 debris
(o 726 '[2 1 2] weak)

; MH370 search days
(o 727 (linear-scale 300 #:lo 0 #:hi 400) weak)

; MH370 search contract
(o 728 (linear-scale 100 #:lo 25 #:hi 275))

; When will the traversal conjecture be proven?
(o 29 '[1 2 3 4 5])

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
(o 103 (yes 5))

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
(o 337 '[.1 1 10 100 1000])

; When will the Rosetta's small robotic lander, Philae, land successfully on the surface of a comet?
(o 349 '[0.1 0.2 10 9 8 7 6 12])

; Will US wind energy consumption decrease in 2014? 
(o 359 (yes 10))

; Will the consumer version of Google Glass be available for purchase in the US before the end of September 2014?
(o 398 (yes 10))

; Will Facebook use drones<E2><80><94>perhaps high-altitude, solar-powered craft like those made by Titan Aerospace<E2><80><94> to provide internet access in places where it is not currently accessible, by the end of December 2017?
(o 531 (yes 10))

; Will Google or Facebook first debut internet connectivity to consumers using solar-powered drones? 
(o 557 '[10 1 10])

; Will a VC-funded Bitcoin business declare bankruptcy by December 31, 2014?
(o 624 (yes 55) weak)

; How many threatened languages will the Ethnologue language catalog report in its 18th edition?
(o 663 '[1 5 30 50 30])

; When will the Bailiwick of Jersey have over 50 companies that accept Bitcoin as a form of payment?
(o 729 '[4 5 5 6])

; When will the Australian Government award the contract for RFT for provision for services relating to the search for MH370?
(o 730
   (map * 
        '[7   7   5   7   7   6   6   14   ]   ; business days in period
        '[ .95 .84 .73 .62 .51 .40 .29  .18])) ; relative likelihood of period

; At its initial launch, will the range of Tesla's Gen 3 electric car be greater than 150 miles? 
(o 731 (yes 60))

; In June 2014, Amazon unveiled the "Fire", the world's first 3-D smartphone. Which of these companies will be next to release a 3-D smartphone? 
(o 732 '[100 199 201])

; Will a Phase 1 clinical trial start recruiting participants before the end of 2014 for a study of the treatment of autism using suramin, a century-old treatment for sleeping sickness?
(o 733 (yes 50) weak)

; Will physicists find any difference in the magnetic moments of the antiproton and the proton when they achieve a direct high-precision measurement of the antiproton?
(o 734 (yes 25))

; When will the Australian contracted search for MH 370 begin?
(o 735 '[14 13 12 10 9 8 7])

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
