#  Function which calculates the values used to determine subjective equivalence bounds for TOST tests. 
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
  
  output$dunb33 <- ( 1 - ( 3 / (4*df - 1) ) ) * output$d33     # Calculate d unbiased for the 33% power criterion
  
  
  ## Original study smallest detectable effect size ##
  
  output$crit.t  <- qt(p = 1-p/2, df = df) # Critical t for original study
  output$crit.d  <- crit.t * sqrt(1/(n1-1) + 1/(n2-1)) # Convert critical t to d
  #output$crit.d2 <- 2*crit.t / sqrt(Shih.df) # Alternative formula. Does not produce the same results when n differs between groups
  return(output)
}

# Test

source(file = "tFromDescTwo.R")
Shih.m1  <-  0.54
Shih.m2  <-  0.49
Shih.sd1 <-  0.17
Shih.sd2 <-  0.20
Shih.n1  <- 16
Shih.n2  <- 16
Shih <- tFromDescTwo(Shih.m1, Shih.m2, Shih.sd1, Shih.sd2, Shih.n1, Shih.n2, test = "Student")

eqboundCritera(Shih.n1, Shih.n2, Shih.df)
