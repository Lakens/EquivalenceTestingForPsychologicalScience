## Example 5 in three easy steps: correlations 

# Every line that starts with a "#" is a comment, and contains information for you.

# 1. Install the TOSTER package. NB! You only need to do this once.
install.packages("TOSTER")

# 2. Load the TOSTER package into your R environment. NB! You only need to do this once every time you open R.
library(TOSTER)

# 3. Run TOST with the numbers from example 5.
# Performing the TOST using the 'TOSTr' function allows you to specify equivalence bounds for a correlation.
# If you want to use this for your own data, simply edit the numbers in the function below to match your data.

TOSTER::TOSTr(n = 231, 
              r = 0.04206585, 
              low_eqbound_r = -0.1830961, 
              high_eqbound_r = 0.1830961, 
              alpha = 0.05)