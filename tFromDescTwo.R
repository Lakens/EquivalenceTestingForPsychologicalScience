# Function for calculating t value, cohens d, pooled standard error, etc. from means, standard deviations and group n's.

tFromDescTwo <- function(m1, m2, sd1, sd2, n1, n2, test = "Welch") {
  
  output        <- list()       # Define the output as list
  output$m1     <- m1           # Store input arguments in output
  output$m2     <- m2
  output$sd1    <- sd1
  output$sd2    <- sd2
  output$n1     <- n1
  output$n2     <- n2
  output$m.diff <- m1 - m2      # Calculate the mean difference
  output$df     <- n1 + n2 - 2  # Calculate degrees of freedom. Welch–Satterthwaite equation could be incorporated as well
  
  if (test == "Student") {  
    # Calculate se.diff, t and d based on a Student t.test
    print("Student's t.test is performed and sample sizes are assumed to be equal")
    output$test       <- "Student"
    output$sd.pooled  <- sqrt( (sd1^2 + sd2^2) / (n1 + n2 - 2) )
    output$se.diff    <- sqrt( ((output$sd1^2) / output$n1) + ((output$sd2^2) / output$n2) ) # Calculate the se of the difference
    
  } else if (test == "Welch") {  
    # Calculate se.diff, t and d based on a welch t.test, and calculate sd.pooled
    print("Welch's t.test is performed and sample sizes are assumed to be unequal")
    output$test        <- "Welch"
    output$sd.pooled  <- sqrt(( (output$n1-1) * (output$sd1^2) ) + ( (output$n2 - 1) * (output$sd2^2) ) / output$df) # Calculate the pooled standard deviation
    output$se.diff    <- sqrt(( (output$sd.pooled^2) / output$n1 ) + ( (output$sd.pooled^2) / output$n2 ))           # Calculate the se of the difference based on sd.pooled
    
    
  } else {
    stop("The 'test' argument must be set to either 'Student'or 'Welch'", call. = FALSE)
  }
  
  output$t    <- output$m.diff / output$se.diff                    # Calculate the t value
  output$d    <- output$t * sqrt( 1 / output$n1 + 1 / output$n2 )  # calculate Cohens d (or Hedges g if "Welch")
  output$dunb <- ( 1 - ( 3 / (4*Moon.df - 1) ) ) * output$d        # Calculate d unbiased (from Cumming 2012, p 294, eq 11.13)
  
  return(output) # Return all items stored in 'output'
}

# Test
Moon.m1  <-  0.46
Moon.m2  <-  0.50
Moon.sd1 <-  0.17
Moon.sd2 <-  0.18
Moon.n1  <- 53
Moon.n2  <- 48

Moon <- tFromDescTwo(Moon.m1, Moon.m2, Moon.sd1, Moon.sd2, Moon.n1, Moon.n2, test = "Student")
