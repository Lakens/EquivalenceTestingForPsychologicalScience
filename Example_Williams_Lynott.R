#  TOST on the values from Lynott, Corker, Wortman, Lynott et al. (2014) example
  # TOST test are calculated using difference between proportions as effect parameter

#### Setup ####

library(TOSTER)
library(pwr)
source("TOSTtwo.prop.R")
source("zDeskProp.R")

#### Data ####

Williams.prop1 <- 0.75
Williams.prop2 <- 0.46
# Total n was 53. From Williams & Barge: Approximately half of the participants briefly held a hot pad, and the remaining participants held a cold pad.
Williams.n1    <- 26 # Approximated
Williams.n2    <- 27 # Approximated

Lynott.prop1  <- 0.512
Lynott.prop2  <- 0.579
Lynott.n1     <- 427
Lynott.n2     <- 434

#### Calculate z for examples ####

Williams <- zDeskProp(prop1 = Williams.prop1, 
                      prop2 = Williams.prop2, 
                      n1    = Williams.n1, 
                      n2    = Williams.n2)

Lynott <- zDeskProp(prop1 = Lynott.prop1, 
                     prop2 = Lynott.prop2, 
                     n1    = Lynott.n1, 
                     n2    = Lynott.n2)

(0.512/0.488)/(0.579/0.421)

#### Calculate subjective equivalence bounds based on small telescopes and smallest detectable effect size ####

#  Small telescopes

h = 0.4174618
# Solve equation in Wolfram using Lynott p1 as p1 and solve for p2
sin(0.797399 - 0.5 * h)^2
# Solve equation in Wolfram using Lynott p1 as p2 and solve for p2
sin(0.5 * h + 0.797399)^2
# Solve equation in Wolfram using Lynott p2 as p1 and solve for p2
sin(0.864731 - 0.5 * h)^2
# Solve equation in Wolfram using Lynott p2 as p2 and solve for p1
sin(0.5 * h + 0.864731)^2


pwr.2p2n.test(n1 = 26, n2 = 27, sig.level = 0.05, power = 0.33, alternative = "two.sided")
pwr.2p.test(sig.level = 0.05, power = 0.33, alternative = "two.sided", n = 3333)


#  Smallest detectable effect size
Lynott$crit.z <- qnorm(1-0.025) #  Calculate critical z for alpha = .05 two-sided
Lynott$crit.diff <- Lynott$crit.z * Lynott$se #  Convert z to proportion difference


#### Calculate TOST with subjective equivalence bounds ####

# Smallest detectable effect size
TOSTtwo.prop(prop1 = Lynott$prop1, prop2 = Lynott$prop2, n1 = Lynott$n1, n2 = Lynott$n2, low_eqbound = -Lynott$crit.diff, high_eqbound = Lynott$crit.diff, alpha = 0.05)

