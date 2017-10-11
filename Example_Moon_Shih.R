##  Example from Dienes (2017): Moon & Roeder (2014) replication of Shih et al. (1999). 

#### Setup ####

library(TOSTER)
library(pwr)
source("Example from Neil Mclathie/2017.10.05 TOSTtwoBayes Raw with Cauchy+HalfCauchy.R")
source("TOSTtwoBayes.R")
source("tFromDescTwo.R")
source("eqboundCritera.R")

#### Data ####

#  Original study summary statistics. 1 = Asian primed women. 2 = unprimed control.
Shih.m1  <-  0.54
Shih.m2  <-  0.49
Shih.sd1 <-  0.17
Shih.sd2 <-  0.20
Shih.n1  <- 16
Shih.n2  <- 16

#  Replication study summary statistics. 1 = Asian primed women. 2 = unprimed control.
Moon.m1  <-  0.46
Moon.m2  <-  0.50
Moon.sd1 <-  0.17
Moon.sd2 <-  0.18
Moon.n1  <- 53
Moon.n2  <- 48

#### Calculate statistics to verify that the values from Dienes are reproducable ####

Moon <- tFromDescTwo(m1   = Moon.m1, 
                     m2   = Moon.m2, 
                     sd1  = Moon.sd1, 
                     sd2  = Moon.sd2, 
                     n1   = Moon.n1, 
                     n2   = Moon.n2, 
                     test = "welch")

Shih <- tFromDescTwo(m1   = Shih.m1, 
                     m2   = Shih.m2, 
                     sd1  = Shih.sd1,
                     sd2  = Shih.sd2, 
                     n1   = Shih.n1, 
                     n2   = Shih.n2, 
                     test = "welch")

#  In Dienes:
  # For Moon & Roeder: t = 1.15, df = 1.15, p = 0.25 - Replicated

#### Calculate subjective equivalence bounds based on small telescopes and smallest detectable effect size ####

Moon$boundcrits <- eqboundCritera(n1 = Shih$n1, n2 = Shih$n2, df = Shih$df)

#### Calculate TOST with subjective equivalence bounds ####

# TOST calculation based on 33% power criterion
  # Not currently able to replicate the Bayes factor reported in Dienes (0.31)
TOSTtwo.raw.nm(m1  = Moon$m1, 
               m2  = Moon$m2, 
               sd1 = Moon$sd1, 
               sd2 = Moon$sd2, 
               n1  = Moon$n1, 
               n2  = Moon$n2, 
               low_eqbound  = -Moon$boundcrits$d33 * Moon$sd.pooled, 
               high_eqbound = Moon$boundcrits$d33 * Moon$sd.pooled, 
               alpha        = .05, 
               prior_dist   = "halfnormal", 
               effect_prior = (Shih.m1 - Shih.m2)) #d converted to raw scores

# TOST for smallest detectable effect in original study
  # Not currently able to replicate the Bayes factor reported in Dienes (0.31)
TOSTtwo.raw.nm(m1  = Moon$m1, 
               m2  = Moon$m2, 
               sd1 = Moon$sd1, 
               sd2 = Moon$sd2, 
               n1  = Moon$n1, 
               n2  = Moon$n2, 
               low_eqbound  = -crit.d*Moon$sd.pooled, 
               high_eqbound = crit.d*Moon$sd.pooled, 
               alpha        = .05, 
               prior_dist   = "halfnormal", 
               effect_prior = (Shih.m1 - Shih.m2)) #d converted to raw scores


#  TOST using d to check that it replicates the test above - It now replicates perfectly
TOSTtwo(m1  = Moon$m1, 
               m2  = Moon$m2, 
               sd1 = Moon$sd1, 
               sd2 = Moon$sd2, 
               n1  = Moon$n1, 
               n2  = Moon$n2, 
               low_eqbound  = -Moon$boundcrits$d33, 
               high_eqbound = Moon$boundcrits$d33, 
               alpha        = .05, 
               prior_dist   = "halfnormal", 
               effect_prior = (Shih.m1 - Shih.m2)) #d converted to raw scores
