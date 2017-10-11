# Function for calculating t value, cohens d, pooled standard error, etc. from means, standard deviations and group n's.

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
    output$sd.pooled  <- sqrt( ( ( ( n1 - 1 ) * ( sd1^2 ) ) + ( n2 - 1 ) * ( sd2^2 ) ) / ( ( n1 + n2 ) - 2 ) ) # Calculate the pooled SD
    output$se.diff    <- output$sd.pooled * sqrt( 1/n1 + 1/n2 ) # Calculate the SE of the difference.
    
  } else if (test == "welch") {  
    # Calculate se.diff, t and d based on a welch t.test, and calculate sd.pooled
    print("welch's t.test is performed and sample sizes are assumed to be unequal")
    output$test       <- "welch"
    output$df         <- ( sd1^2 / n1 + sd2^2 /n2 )^2 / ( ( ( sd1^2 / n1 )^2 / ( n1-1 ) ) + ( ( sd2^2 / n2 )^2 / ( n2 - 1 ) ) ) #degrees of freedom for Welch's t-test
    output$sd.pooled  <- sqrt( ( sd1^2 + sd2^2 ) / 2 )    # Calculate sd root mean squared for Welch's t-test
    output$se.diff    <- sqrt( sd1^2 / n1 + sd2^2 / n2 )  # Calculate the se of the difference based on sd.pooled
    
    
  } else {
    stop("The 'test' argument must be set to either 'student'or 'welch'", call. = FALSE)
  }
  
  output$t    <- output$m.diff / output$se.diff                      # Calculate the t value
  output$p    <- 2*pt( q = -abs( output$t ), df = 99)
  output$d    <- output$t * sqrt( 1 / output$n1 + 1 / output$n2 )    # calculate Cohens d (or Hedges g if "welch")
  output$dunb <- ( 1 - ( 3 / (4*output$df - 1) ) ) * output$d        # Calculate d unbiased (from Cumming 2012, p 294, eq 11.13)
  
  return(output) # Return all items stored in 'output'
}

# Test
Moon.m1  <-  0.46
Moon.m2  <-  0.50
Moon.sd1 <-  0.17
Moon.sd2 <-  0.18
Moon.n1  <- 53
Moon.n2  <- 48

Moon <- tFromDescTwo(Moon.m1, Moon.m2, Moon.sd1, Moon.sd2, Moon.n1, Moon.n2, test = "student")