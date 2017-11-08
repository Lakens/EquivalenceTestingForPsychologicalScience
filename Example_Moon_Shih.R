##  Example from Dienes (2017): Moon & Roeder (2014) replication of Shih et al. (1999). 

#### Setup ####

library(TOSTER)
library(pwr)
#source("Example from Neil Mclathie/2017.10.05 TOSTtwoBayes Raw with Cauchy+HalfCauchy.R")
#source("TOSTtwoBayes.R")

#### Create functions for calculating statistics ------------------------------

# Create function for calculating t value, cohens d, pooled standard error, etc. from means, standard deviations and group n's.

tFromDescTwo <- function(m1, m2, sd1, sd2, n1, n2, test = "welch") {
  
  output        <- list()       # Define the output as list
  output$m1     <- m1           # Store input arguments in output
  output$m2     <- m2
  output$sd1    <- sd1
  output$sd2    <- sd2
  output$n1     <- n1
  output$n2     <- n2
  output$m.diff <- m1 - m2      # Calculate the mean difference
  
  
  if (test == "student") {  
    # Calculate se.diff, t and d based on a student t.test
    print("student's t.test is performed and sample sizes are assumed to be equal")
    output$test       <- "student"
    output$df         <- n1 + n2 - 2  # Calculate degrees of freedom.
    output$sd.pooled  <- sqrt(((( n1 - 1) * (sd1^2)) + (n2 - 1) * (sd2^2)) / ((n1 + n2) - 2 )) # Calculate the pooled SD
    output$se.diff    <- output$sd.pooled * sqrt(1/n1 + 1/n2) # Calculate the SE of the difference.
    
  } else if (test == "welch") {  
    # Calculate se.diff, t and d based on a welch t.test, and calculate sd.pooled
    print("welch's t.test is performed and sample sizes are assumed to be unequal")
    output$test       <- "welch"
    output$df         <- (sd1^2 / n1 + sd2^2 /n2)^2 / (((sd1^2 / n1)^2 / (n1-1)) + ((sd2^2 / n2)^2 / (n2 - 1))) #degrees of freedom for Welch's t-test
    output$sd.pooled  <- sqrt((sd1^2 + sd2^2) / 2)    # Calculate sd root mean squared for Welch's t-test
    output$se.diff    <- sqrt(sd1^2 / n1 + sd2^2 / n2)  # Calculate the se of the difference based on sd.pooled
    
    
  } else {
    stop("The 'test' argument must be set to either 'student'or 'welch'", call. = FALSE)
  }
  
  output$t    <- output$m.diff / output$se.diff                      # Calculate the t value
  output$p    <- 2*pt(q = -abs(output$t), df = 99)
  output$d    <- output$t * sqrt(1 / output$n1 + 1 / output$n2)    # calculate Cohens d (or Hedges g if "welch")
  output$dunb <- (1 - (3 / (4*output$df - 1))) * output$d        # Calculate d unbiased (from Cumming 2012, p 294, eq 11.13)
  
  return(output) # Return all desired output statistics stored in 'output'
}





#  Create function which calculates the values used to determine subjective equivalence bounds for TOST tests. 
# Calculates the effect size (Cohens d) that a study has % power (33% by default) to detect.
# Calculates the the smallest detectable effect size of a study based on the degrees of freedom and p value (0.05 by default).

eqboundCritera <- function(n1, n2, df, p = 0.05, power = 0.33, testtype = "two.sided", var.equal = FALSE) {
  output <- list()  # Define output as list
  
  # Calculate d that study has 33% chance 
  output$d33    <- pwr::pwr.t2n.test(n1          = n1, 
                                     n2          = n2, 
                                     power       = power, 
                                     sig.level   = p, 
                                     alternative = testtype)$d # Calculate effect size that study have 33% power to detect
  
  output$dunb33 <- (1 - (3 / (4*df - 1))) * output$d33     # Calculate d unbiased for the 33% power criterion
  
  
  ## Original study smallest detectable effect size ##
  
  output$crit.t  <- qt(p = 1-p/2, df = df) # Critical t for original study
  output$crit.d  <- output$crit.t * sqrt(1/(n1-1) + 1/(n2-1)) # Convert critical t to d
  #output$crit.d2 <- 2*crit.t / sqrt(Shih.df) # Alternative formula. Does not produce the same results when n differs between groups
  return(output)
}

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
                     test = "student")

#  In Dienes:
  # For Moon & Roeder: t = 1.15, df = 1.15, p = 0.25 - Replicated

#### Calculate subjective equivalence bounds based on small telescopes and smallest detectable effect size ####

Moon$boundcrits <- eqboundCritera(n1 = Shih.n1, n2 = Shih.n2, df = Shih.df, testtype = "greater")

#### Calculate TOST with subjective equivalence bounds ####

# TOST calculation based on 33% power criterion
  # Not currently able to replicate the Bayes factor reported in Dienes (0.31)
# TOSTtwo.raw.nm(m1  = Moon$m1, 
#                m2  = Moon$m2, 
#                sd1 = Moon$sd1, 
#                sd2 = Moon$sd2, 
#                n1  = Moon$n1, 
#                n2  = Moon$n2, 
#                low_eqbound  = -Moon$boundcrits$d33 * Moon$sd.pooled, 
#                high_eqbound = Moon$boundcrits$d33 * Moon$sd.pooled, 
#                alpha        = .05, 
#                prior_dist   = "halfnormal", 
#                effect_prior = (Shih.m1 - Shih.m2)) #d converted to raw scores

# TOST for smallest detectable effect in original study
  # Not currently able to replicate the Bayes factor reported in Dienes (0.31)
TOSTtwo(m1  = Moon$m1,
               m2  = Moon$m2,
               sd1 = Moon$sd1,
               sd2 = Moon$sd2,
               n1  = Moon$n1,
               n2  = Moon$n2,
               low_eqbound  = -crit.d,
               high_eqbound = crit.d,
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
