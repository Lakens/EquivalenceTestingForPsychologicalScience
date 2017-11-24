

## Example 2 in three easy steps: t-test for two independent groups.


# Every line that starts with a "#" is a comment, and contains information for you.

# 1. Install the TOSTER package. NB! You only need to do this once.
install.packages("TOSTER")

# 2. Load the TOSTER package into your R environment. NB! You only need to do this once every time you open R.
library(TOSTER)

# 3. Run TOST with the numbers from example 2.
# Performing the TOST using the 'TOSTtwo' function allows you to specify equivalence bounds in Cohen's d.
# If you want to use this for your own data, simply edit the numbers in the function below to match your data.

TOSTtwo(m1  = 4.785714,  # Mean of group 1
        m2  = 4.656863,  # Mean of group 2
        sd1 = 1.089725,  # Standard deviation of group 1
        sd2 = 1.189497,  # Standard deviation of group 2
        n1  = 49,  # Number of subjects in group 1
        n2  = 51,  # Number of subjects in group 2
        low_eqbound_d  = -0.4929019,  # Value for the lower equivalence bound
        high_eqbound_d =  0.4929019,  # Value for the higher equivalence bound
        alpha = .05)  # Alpha level for TOST and NHST
