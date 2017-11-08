# TOST on the values from Gibson, Losee, and Vitiello example

#### Setup ####

library(TOSTER)
library(pwr)
source("Example from Neil Mclathie/2017.10.05 TOSTtwoBayes Raw with Cauchy+HalfCauchy.R")
source("TOSTtwoBayes.R")
source("tFromDescTwo.R")
source("eqboundCritera.R")

#### Data ####

Gibson.m1  <-  0.63
Gibson.m2  <-  0.51
Gibson.sd1 <-  0.22
Gibson.sd2 <-  0.23
Gibson.n1  <- 40
Gibson.n2  <- 43

Gibson.se.pool <- sqrt((Gibson.sd1^2 + Gibson.sd2^2) / 2)

Shih.m1  <-  0.54
Shih.m2  <-  0.43
Shih.sd1 <-  0.17
Shih.sd2 <-  0.16
Shih.n1  <- 16
Shih.n2  <- 14

Shih.se.pool <- sqrt( (Shih.sd1^2 + Shih.sd2^2) / 2 )
Shih.t <- 2.02 # Stated in Shih paper, but is not reproduced by an independent sample t.test. 
Shih.d <- Shih.t*sqrt(1/(16-1) + 1/(14-1))
Shih.df <- Shih.n1+Shih.n2-2


#### Calculate statistics to verify that the values from Dienes are reproducable ####

Gibson <- tFromDescTwo(m1   = Gibson.m1, 
                       m2   = Gibson.m2, 
                       sd1  = Gibson.sd1, 
                       sd2  = Gibson.sd2, 
                       n1   = Gibson.n1, 
                       n2   = Gibson.n2, 
                       test = "student")

Shih <- tFromDescTwo(m1   = Shih.m1, 
                     m2   = Shih.m2, 
                     sd1  = Shih.sd1,
                     sd2  = Shih.sd2, 
                     n1   = Shih.n1, 
                     n2   = Shih.n2, 
                     test = "student")

#  In Dienes:
  # For Shih, Pittinsky & Ambady:   t = 2.02, df = 29, p = 0.53 - Not replicated (t = 1.82, df = 28, p = 0.07)
  # For Gibson, Losee and Vitiello: t = 2.40, df = 81, p = 0.02 - Replicated

#### Calculate subjective equivalence bounds based on small telescopes and smallest detectable effect size ####

Gibson$boundcrits <- eqboundCritera(n1 = Shih$n1, n2 = Shih$n2, df = Shih$df)

#### Calculate TOST with subjective equivalence bounds ####

#  TOST calculation based on 33% power criterion
  # Not currently able to replicate the Bayes factor reported in Dienes (4.50)
TOSTtwo.raw.nm(m1  = Gibson$m1, 
               m2  = Gibson$m2, 
               sd1 = Gibson$sd1, 
               sd2 = Gibson$sd2, 
               n1  = Gibson$n1, 
               n2  = Gibson$n2, 
               low_eqbound  = -Gibson$boundcrits$d33*Gibson$sd.pooled, 
               high_eqbound = Gibson$boundcrits$d33*Gibson$sd.pooled, 
               alpha        = .05, 
               prior_dist   = "halfnormal", 
               effect_prior = (Shih.m1 - Shih.m2)) #d converted to raw scores

#  TOST for smallest detectable effect in original study
  # Not currently able to replicate the Bayes factor reported in Dienes (4.50)
TOSTtwo.raw.nm(m1  = Gibson$m1, 
               m2  = Gibson$m2, 
               sd1 = Gibson$sd1, 
               sd2 = Gibson$sd2, 
               n1  = Gibson$n1, 
               n2  = Gibson$n2, 
               low_eqbound  = -crit.d*Gibson$sd.pooled, 
               high_eqbound = crit.d*Gibson$sd.pooled, 
               alpha        = .05, 
               prior_dist   = "halfnormal", 
               effect_prior = (Shih.m1 - Shih.m2)) #d converted to raw scores
