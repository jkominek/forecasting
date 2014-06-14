from datetime import *

opinions = { }
boring = set([455, 351, 454, 447, 393, 394,  29,
              216, 588, 658, 212, 481, 495, 518,
              490, 524, 526, 527, 502, 480, 528,
              590, 520, 703, 708, 477, 479, 513,
              500, 492, 487, 199, 538, 537, 504,
              497, 486, 471, 501, 519, 522, 503,
              650, 651, 652])

weak = 0.5
strong = 2.0

def varying_strength(start_dt, stop_dt, start_str=weak, stop_str=strong):
    scale = (date.today() - start_dt).total_seconds() / (stop_dt - start_dt).total_seconds()
    strength = scale * (stop_str - start_str) + start_str
    strength = max(0.01, strength)
    return strength

def normalize_beliefs(beliefs):
    total = float(sum(beliefs))
    return map(lambda b: b/total, beliefs)

def o(key, beliefs, strength=1.0):
    opinions[key] = (normalize_beliefs(beliefs), strength)

def goes_to(f, initial=None, start=None, stop=None, flat_before=True):
    if flat_before and date.today() < start:
        return f(initial)
    scale = (date.today() - start).total_seconds() / (stop - start).total_seconds()
    percent = scale * (100.0 - initial) + initial
    return f(percent)

def yes(percent):
    return [100-percent, percent]

def no(percent):
    return [percent, 100-percent]

# what happened to mh370
o(365,  [ 0,    0,    0,    0,    0,    100], strong)

# Will Nature retract one or more of the January 2014 papers by H. Obokata et al. describing stimulus-triggered acquisition of pluripotency (STAP)?
o(395, yes(70), weak)

# Will scientists create a fully air-transmissible, mammalian-infectious strain of avian influenza in a laboratory setting by the end of 2014?
o(656, [1, 5], weak)

# Will Jupiter's Great Red Spot shrink below 9,000 miles in diameter before January 1, 2016?
o(684, goes_to(no, 80, date(2014,5,1), date(2015,12,1)),
  varying_strength(date(2014,5,1), date(2015,12,1)))

# stable transactinide
o(678, [1, 1, 1, 1, 1,
        1, 1, 1, 1, 1,
        1, 1, 1, 0.01, 0.1,
        0.01, 0.01, 0.01, 0.01, 0.01,
        0.01, 0.01, 0.01, 0.001, 5])

# Will Virgin Galactic begin commercial flights aboard SpaceShipTwo by the end of 2014?
o(670, no(60), varying_strength(date(2014,6,1), date(2014,11,1)))

# Will a wearable camera with speed of more than 60 frames per second at 4K resolution be commercially available by the end of 2015?
o(400, yes(60), varying_strength(date(2014,6,1), date(2015,10,1)))

# Will implantable sensors for monitoring cancer enter clinical trials by the end of 2015?
o(411, yes(33.3))

# Will a dishonest Bitcoin mining pool successfully execute a 51% attack by December 31, 2015?
o(342, yes(15), varying_strength(date(2014,5,1), date(2015,12,1), weak, strong))

# Will a cellphone that charges itself using self-charging power cells (SCPC) be demonstrated publicly before the end of 2015?
o(427, yes(49), weak)

# Will DARPA's Mobile Hotspot program enter Phase 3 by the end of June 2015?
o(681, yes(45), weak)

# When will a transactinide isotope with a half-life longer than 29 hours be discovered?
o(680, [1, 1.5, 2, 2.5, 3], strong)

# Will Amazon offer on-demand music streaming, as part of its Prime Service, by the end of July 2014? 
#o(671, yes(100), strong)

# How fast will the next 100 meter human sprinting world record be?
# 2015 world championship in athletics is in beijing, ending sept 6th 2015
# unlikely (impossible?) any record could be set in 2015 after that.
o(666, [0.1, 0.11, 0.12, 0.15, 1, 5],
  varying_strength(date(2014,9,1), date(2015,9,6)))

# Will Google announce development of a smartwatch at or before the Google I/O 2014 Conference?
o(672, yes(70))

# Will there be at least one female winner of a Fields Medal in 2014?
o(462, yes(20), weak)

# Will Google make end-to-end PGP encryption a core feature in Gmail by August 2014?
o(585, no(98), varying_strength(date(2014,6,1), date(2014,7,25), weak*1.5, strong))

# When will the Deque Conjecture be proven?
o(26, [1, 10, 10, 10, 20], weak)

# How many female chess players will hold a rating of at least 2700 on JUL 01 2016?
o(318, [10, 5, 1, 0.01, 0.001], weak)

# Will Apple announce development of an iWatch at or before the 2014 Worldwide Developer's Conference (WWDC)?
#o(444, no(99), strong)

# Will the Apple iWatch be commercially available by the end of September 2014?
o(401, no(90), varying_strength(date(2014,6,5), date(2014,8,20), weak, strong))

# Will Bluefin-21 locate the black box from Malaysian Airlines flight MH-370?
o(464, no(99), strong)

# Will 23andMe offer health-related genetic reports for new customers by December 2014?
o(659, yes(50), weak)

# Will the International Sun-Earth Explorer 3 be successfully commanded to fire its thrusters by researchers?
o(643, yes(76), weak)

# Will the International Sun-Earth Explorer 3 start to collect data and send it to Earth?
o(644, yes(65), weak)

# Will NASA land a telepresence robot on Venus by 2020?
o(653, no(98))

# When will UHD programming be available from Netflix on optical disc?
o(587, [9, 20, 25, 46])

# When will a metropolitain area of over 250,000 people wirelessly network all traffic signals and sensors so that an AI system can optimize 2-dimensional traffic flow in real-time?
o(636, [1, 10, 10, 10, 10, 10])

# Will a solar powered Apple laptop with photovoltaic cells located in the display module be commercially available before the end of 2018?
o(330, no(90), weak)

# Will there be a new digital currency based on a P2P network, whose real money supply will be valued at more than $1 billion in 2014?
o(102, yes(33), weak)

# Will Apple integrate a sapphire crystal solar power screen in its new iPhone6, due to be released later in 2014?
o(354, no(95), strong)

# Will a VC-funded Bitcoin business initiate a public IPO by December 31, 2014?
o(625, no(85), weak)

# When will a single confirmed exploitation of the Heartbleed bug result in exposure of personally identifiable information (PII) of more than 1 million users?
o(539, [1, 1, 1, 1, 1, 50])

# Will quantum key distribution be integrated into a prototype mobile device by 1 January 2016?
o(562, no(75), weak)

# When will the first U.S. state enact legislation allowing consumer operation of driverless cars?
o(149, [1, 1.5, 2, 2.5, 3], weak)

# Will the Axion Dark Matter Experiment detect axions by July 2014?
o(434, no(98), strong)

# When will the first chess player achieve a rating of at least 2900?
o(313, [2, 2, 2, 2, 1])

# Will Paypal integrate Bitcoin payments by February 28, 2015?
o(341, no(90), weak)

# Will Google integrate Bitcoin payments into Google Wallet by February 28, 2015?
o(338, no(90), weak)

# Will either the United States or China be able to build an encryption cracking quantum computer before the end of 2018?
o(436, yes(15))

# Will the identity of Satoshi Nakamoto be identified by December 31, 2014?
o(418, yes(10))

# Will there be a hard fork to the Bitcoin block chain by December 31, 2014?
o(459, yes(20))

# Will an orbiting body system be discovered in space before the end of 2019 which could verify at least one of the 13 new three-body problem solution families recently discovered by two Belgrade physicists?
o(442, no(90), strong)

# Will quantum key distribution be integrated into a commercially available mobile device by 1 January 2017?
o(534, no(90), varying_strength(date(2015,1,1), date(2016,12,1), weak, 2*strong))

# Where will the Malaysia Airlines Flight MH370 be found?
#o(364, [1, 1, 1, 1, 1, 1, 1000], strong)

# When will the first mass-produced multicopter, for human flight, be offered for sale?
o(182, [1, 2, 3, 5])

# Will a breathalyzer for measuring blood sugar be commercially available by the end of 2016?
o(402, yes(40), weak)

# Will a solar cell with efficiency of at least 50% be reported by the end of 2017?
o(424, no(66))

# Will project NA62 at the CERN in Geneva make a discovery of new unknown particles not predicted by the Standard Model of particle physics in 2014?
o(183, no(98), weak)

# What will the unemployment rate for ACS chemists be in 2014?
o(348, [1, 2, 1, 0.5, 0.5, 0.1], weak)

# What kind of a computing device will be used to create the first 8-man chess endgame tablebase?
o(312, [1, 100, 1, 1, 10], varying_strength(date(2014,1,1), date(2020,1,1), weak/4, strong))

# When will the Clay Mathematics Institute announce a winner of the Millennium Prize for resolving the "P vs NP Problem"?
o(27, [1, 2, 3, 4, 50], varying_strength(date(2045,1,1), date(2050,1,1), weak, strong))

# Which of the following changes will be reported about "Status and trends of linguistic diversity and numbers of speakers of indigenous languages" in the fourth edition of the Global Biodiversity Outlook report?
o(99, [0.5, 10, 1, 0.1], weak*0.5)

# Will the NASA's Mars Atmosphere and Volatile Evolution (MAVEN) spacescraft launched on November 17, 2013 enter the Mars orbit by the end of September 2014?
o(143, yes(95))

# Will the silver nanowire ink touch sensitive screens being developed by 3M and Cambrios be in commercially available smartphones by the end of 2015?
o(363, yes(33), weak)

# Will the 9-man chess endgame tablebase be created before JAN 01 2030?
o(311, yes(50), varying_strength(date(2014,6,1), date(2029,1,1)))

# Will the trade price of Whole Milk Powder fall below $3000 per tonne by December 31, 2014?
o(685, yes(55), weak)

# (3) id 421 "Will the unit price of Chinese solar PV modules fall below 50 cents per watt in the US by the end of 2015?"
o(421, no(51), varying_strength(date(2014,6,10), date(2014,12,31), strong, weak))

# Will planet Kepler 62f be found to have water on its surface?
o(608, no(40), weak/2.0)

# Will a commercial building with perovskite solar cell windows be completed by the end of 2018? 
o(329, goes_to(yes, 40, date(2014,6,1), date(2018,8,1)),
  varying_strength(date(2014,6,1), date(2018,7,1), 0.8, 1.2))

# A young paraplegic Brazilian, assisted by a neurorobotic exoskeleton, will take the ceremonial first kick at the 2014 World Cup in Sao Paulo, Brazil. How far will the ball travel? 
#o(621, [10, 9, 8, 7, 6, 5, 4, 3], weak)

# When will the first Hyperloop-like system begin public operations?
o(181, [1, 2, 3, 4, 5, 45], weak)

# "By the end of 2014, will there be at least three US states that tax e-cigarettes?"
o(686, yes(55), varying_strength(date(2014, 6, 1), date(2014,12,1)))

# At its first launch, will Apple's iWatch include a sensor to measure blood glucose level non-invasively?
o(687, no(66.666))

# Will Google's glucose monitoring "smart" contact lenses get FDA approval by the end of 2016?
o(688, no(49), varying_strength(date(2014,7,1), date(2015,7,1)))

# Will a foldable tablet computer made with inkjet-printed graphene be commercially available by the end of 2018? 
o(453, goes_to(no, 66.666, date(2014,6,15), date(2017,10,1)),
  varying_strength(date(2014,8,1), date(2017,11,1)))

# On June 2014, what will be the cores per socket of the Top 500 winner? 
o(41, [1, 2, 20*0.8, 2, 1], strong)

# On June 2014, what will be the geographic region of the Top 500 winner? 
o(43, [2, 10*0.8, 1, 1, 1], strong)

# On June 2014, what will be the vendor of the Top 500 winner? 
o(39, [2, 2, 1, 1, 50], strong)

# On June 2014, what will be the processor generation of the Top 500 winner? 
o(42, [1, 2, 20*0.9, 1, 1], strong)


# On November 2014, what will be the cores per socket of the Top 500 winner? 
o(45, [1, 2, 20*.5, 2, 1], strong*0.55)

# On November 2014, what will be the geographic region of the Top 500 winner? 
o(44, [2, 10, 1, 1, 1], strong*0.55)

# On November 2014, what will be the vendor of the Top 500 winner? 
o(43, [2, 3, 1, 1, 20*.5], strong*0.55)

# On November 2014, what will be the processor generation of the Top 500 winner? 
o(46, [1, 2, 20*.5, 1, 1], strong*0.55)

# Will the Axion Dark Matter Experiment detect dark matter axions by the end of 2014?
o(128, no(95))

# Will lab experiments conducted with Josephson junctions prove the existence of axions by the end of 2015?
o(227, no(90), weak)

# Will China have at least 30 nuclear power reactors in operation by the start of 2015?
o(173, no(95))

# Will evidence of proton decay be reported in the scientific literature by the end of 2017?
o(508, no(95), weak)

# Will the Large Underground Xenon dark matter experiment in Lead, SD detect the dark matter particles weakly interacting massive particles (WIMPs) or weakly interacting slim particles (WISPs) by the end of 2014?
o(145, no(95), weak)

# (10) id 438 "Will GE and Sasol's new AbMBR technology, which cleans waste water while generating biogas for power production, be comercially available by the end of June 2015?"
o(438, yes(45),
  varying_strength(date(2014,7,1), date(2015,5,15)))

# On June 2014, what will be the (performance-weighted) gigaflops per watt of the Top 500?
o(72, [95, 2, 1, 1, 1], strong)

# On NOV 2014, what will be the (performance-weighted) gigaflops per watt of the Top 500?
o(175, [94, 3, 1, 1, 1], strong*0.666)

# In the week of November 23rd-29th 2014, will Walmart be searched more frequently than Amazon in Google Search?
o(649, yes(90))

# when will a fully autonomous (self-driving) car made by a major auto maker drive more than 60 miles in the united states without input from the driver?
o(696, [2, 3, 4, 3, 2, 1], weak)

# On June 2014, what will be the Top 500 performance share by geographic region?
performance = [1.1 * 120339438,
               1.11 * 72809408,
               1.08 * 31768483,
               1.08 * 11670158,
               1.05 * (3040297+3864640+2302522+2180151+1479371) ]
performance = map(float, performance)
o(54, performance)

# On November 2014, what will be the Top 500 performance share by geographic region?
performance = [1.10**2 * 120339438,
               1.11**2 * 72809408,
               1.08**2 * 31768483,
               1.08**2 * 11670158,
               1.05**2 * (3040297+3864640+2302522+2180151+1479371) ]
performance = map(float, performance)
o(48, performance)

# On June 2014, what will be the Top 500 performance share by cores per socket?
performance = [ (0.98 * 6054483 + 25323699),
                97765893,
                (6291384 + 42786013),
                1.02 * 71736595,
                10000000 ]
o(55, performance)

# Will Amazon deliver its first package using an unmanned aerial vehicle by DEC 31 2017?
o(105, yes(28), weak)

# Which of the following 2016 presidential candidates will be the first to accept Bitcoin campaign contributions? 
o(371, [5, 1, 2, 20, 5, 2, 20], weak)

# Will the June 2014 TOP500 rankings announce that the performance of the #500 ranked supercomputer has exceeded 150 thousand Gflops/s?
o(14, yes(10), 0.2)

# Will the Mars Curiosity Rover discover organic matter on Mars-evidence that life exists or existed on the planet-by July 1, 2015?
o(377, no(98))

# When will the Chinese National Space Administration land a man or woman on the moon?
o(136, [1, 9, 10, 10])

# Will the Chinese National Space Administration retrieve at least 2kg of lunar rock/soil samples by January 1 2018? 
o(135, yes(15), varying_strength(date(2014,6,15), date(2017,10,1)))
