# Load TOSTER
library(TOSTER)

# Calculate the smallest effect sizer of interest (crit.diff), used for setting the bounds.
se <- sqrt((0.75 * (1 - 0.75)) / 26 + (0.46 * (1 - 0.46)) / 27)  # Calculate the (unpooled) standard error.
crit.z <- qnorm(1 - (0.05 / 2))  # Determine the critical z for a two-tailed test with alpha = 0.05.
crit.diff <- crit.z * se  # Convert z to the critical difference.

#  Load the data and exclude cases to only include cases that did not meet the authors' exclusion criteria.
lynott.data <- read.csv("Example4_Lynott-et-al_data-cleaned.csv")
lynott.data <- lynott.data[lynott.data$include1 == 1,]

#  Calculate the summary statistics from the data that we need in the TOST calculation below.
n.hot  <- sum(lynott.data$packtype == 1)
n.cold <- sum(lynott.data$packtype == 0)
selfish.hot  <- sum(lynott.data$selfish[lynott.data$packtype == 1] == 1, na.rm = TRUE) / n.hot
selfish.cold <- sum(lynott.data$selfish[lynott.data$packtype == 0] == 1, na.rm = TRUE) / n.cold
diff <- selfish.hot - selfish.cold

#  Calculate equivalence test for two proportions.
Lynott <- TOSTtwo.prop(prop1 = selfish.cold,
                     prop2 = selfish.hot,
                     n1    = n.cold,
                     n2    = n.hot,
                     low_eqbound  = -crit.diff,
                     high_eqbound = crit.diff,
                     alpha = 0.05)