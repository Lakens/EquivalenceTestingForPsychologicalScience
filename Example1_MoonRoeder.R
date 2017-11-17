# Load TOSTER into R environment 
library(TOSTER)

#  Load the relevant datafile into R
moon.data <- read.csv("Example1_MoonRoeder_data-cleaned.csv")  # Data can be found at https://osf.io/rwv5s/ (final_dataset_139_to_send.sav)

#  Set the bounds, in this case 1 grade point, which corresponds to 6.25%
sesoi <- 0.0625

#  Calculate the summary statistics from the data that we need in the TOST calculation below
mean.asian <- mean(moon.data$accuracy[moon.data$identity_salience == 1])
mean.control <- mean(moon.data$accuracy[moon.data$identity_salience == 2])
sd.asian <- sd(moon.data$accuracy[moon.data$identity_salience == 1])
sd.control <- sd(moon.data$accuracy[moon.data$identity_salience == 2])
n.asian <- sum(moon.data$identity_salience == 1)
n.control <- sum(moon.data$identity_salience == 2)
sample.sd <- sqrt((sd.asian^2 + sd.control^2) / 2)  # Calculate sd root mean squared for Welch's t-test
sample.se <- sqrt(sd.asian^2 / n.asian + sd.control^2 / n.control)  # Calculate the se of the difference based on sd.pooled

df <- (sd.asian^2 / n.asian + sd.control^2 / n.control)^2 / (((sd.asian^2 / n.asian)^2 / (n.asian-1)) + ((sd.control^2 / n.control)^2 / (n.control - 1)))  # Degrees of freedom for Welch's t-test
t <- (mean.asian - mean.control) / sample.se  # Welch t-test
p <- 2 * pt(t, df)
sdpooled <- sqrt((((n.asian - 1) * (sd.asian^2)) + (n.control - 1) * (sd.control^2)) / ((n.asian + n.control) - 2))
d <- (mean.asian - mean.control) / sdpooled


#  Performing the equivalence test
Moon <- TOSTER::TOSTtwo.raw(m1  = mean.asian,
                            m2  = mean.control,
                            sd1 = sd.asian,
                            sd2 = sd.control,
                            n1  = n.asian,
                            n2  = n.control,
                            low_eqbound  = -sesoi,
                            high_eqbound =  sesoi,
                            alpha = .05)
