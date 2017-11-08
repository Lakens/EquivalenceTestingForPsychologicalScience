#### Fisher's exact Z Test for proportions - from PASS Sample Size Software ch. 213 ####

zDeskProp <- function(prop1, prop2, n1, n2) {
  
  diff <- prop1 - prop2  # Calculate difference
  se <- sqrt( ( prop1 * ( 1 - prop1 ) ) / n1 + ( prop2 * ( 1 - prop2 ) ) / n2 )  # Calculate standard error based on Fisher z test
  z = ( diff - 0 ) / se # Calculate z score of the difference
  
  output <- list()
  output$diff <- diff
  output$se <- se
  output$z <- z
  output$prop1 <- prop1
  output$prop2 <- prop2
  output$n1 <- n1
  output$n2 <- n2
  
  return(output)
}


#### test ####

zDeskProp(.512, .579, 422, 428)
