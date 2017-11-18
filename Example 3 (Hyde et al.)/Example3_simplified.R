

## Example 3 in three easy steps: meta-analysis on Cohen's d.


# Every line that starts with a "#" is a comment, and contains information for you.

# 1. Install the TOSTER package. NB! You only need to do this once.
install.packages("TOSTER")

# 2. Load the TOSTER package into your R environment. NB! You only need to do this once every time you open R.
library(TOSTER)

# 3. Run TOST with the numbers from example 3, for Grade 3.
# Performing the TOST using the 'TOSTmeta' function allows you to specify equivalence bounds in Cohen's d.
# If you want to use this for your own data, simply edit the numbers in the function below to match your data.

TOSTmeta(ES = 0.04,  # Meta-analytic effect size
         se = 0.002,  # Standard deviation
         low_eqbound_d = -0.1,  # Value for the lower equivalence bound
         high_eqbound_d = 0.1,  # Value for the higher equivalence bound
         alpha = 0.05)  # Alpha level for TOST and NHST