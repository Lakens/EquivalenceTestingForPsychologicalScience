## Example 5 in three easy steps: correlations.

# Every line that starts with a "#" is a comment, and contains information for you.

# 1. Install the TOSTER package. NB! You only need to do this once.
install.packages("TOSTER")

# 2. Load the TOSTER package into your R environment. NB! You only need to do this once every time you open R.
library(TOSTER)

# 3. Run TOST with the numbers from example 5.
# Performing the TOST using the 'TOSTr' function allows you to specify equivalence bounds for a correlation.
# If you want to use this for your own data, simply edit the numbers in the function below to match your data.

TOSTr(n = 231,  # Number of pairs of observations
      r = -0.04206585,  # Correlation effect size
      low_eqbound_r = -0.1830961,  # Value for the lower equivalence bound
      high_eqbound_r = 0.1830961,  # Value for the higher equivalence bound
      alpha = 0.05)  # Alpha level for TOST and NHST