

## Example 1 in three easy steps: t-test for two independent groups.


# Every line that starts with a "#" is a comment, and contains information for you.

# 1. Install the TOSTER package. NB! You only need to do this once.
install.packages("TOSTER")

# 2. Load the TOSTER package into your R environment. NB! You only need to do this once every time you open R.
library(TOSTER)

# 3. Run TOST with the numbers from example 1.
# Performing the TOST using the 'TOSTtwo.raw' function allows you to specify equivalence bounds in raw values.
# If you want to use this for your own data, simply edit the numbers in the function below to match your data.

TOSTtwo.raw(m1  = 0.458513,   # Mean of group 1
            m2  = 0.4962693,  # Mean of group 2
            sd1 = 0.1749011,  # Standard deviation of group 1
            sd2 = 0.1770452,  # Standard deviation of group 2
            n1  = 53,  # Number of subjects in group 1
            n2  = 48,  # Number of subjects in group 2
            low_eqbound  = -0.0625,  # Value for the lower equivalence bound
            high_eqbound =  0.0625,  # Value for the higher equivalence bound
            alpha = .05)  # Alpha level for TOST and NHST
