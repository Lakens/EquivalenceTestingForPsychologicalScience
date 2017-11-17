## Example 3 in three easy steps: meta-analysis on Cohen's d

# Every line that starts with a "#" is a comment, and contains information for you.

# 1. Install the TOSTER package. NB! You only need to do this once.
install.packages("TOSTER")

# 2. Load the TOSTER package into your R environment. NB! You only need to do this once every time you open R.
library(TOSTER)

# 3. Run TOST with the numbers from example 3, for Grade 3.
# Performing the TOST using the 'TOSTmeta' function allows you to specify equivalence bounds in Cohen's d.
# If you want to use this for your own data, simply edit the numbers in the function below to match your data.

TOSTER::TOSTmeta(ES = 0.04, 
                 se = 0.002, 
                 low_eqbound_d = -0.1, 
                 high_eqbound_d = 0.1, 
                 alpha = 0.05, 
                 plot = TRUE)