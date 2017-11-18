## Example 4 in three easy steps: comparing two independent proportions.


# Every line that starts with a "#" is a comment, and contains information for you.

# 1. Install the TOSTER package. NB! You only need to do this once.
install.packages("TOSTER")

# 2. Load the TOSTER package into your R environment. NB! You only need to do this once every time you open R.
library(TOSTER)

# 3. Run TOST with the numbers from example 4.
# Performing the TOST using the 'TOSTtwo.prop' function allows you to specify equivalence bounds in proportions.
# If you want to use this for your own data, simply edit the numbers in the function below to match your data.

TOSTtwo.prop(prop1 = 0.5074257,  # Proportion of group 1
             prop2 = 0.5745721,  # Proportion of group 2
             n1    = 404,  # Number of subjects in group 1
             n2    = 409,  # Number of subjects in group 2
             low_eqbound  = -0.2510861,  # Value for the lower equivalence bound
             high_eqbound = 0.2510861,  # Value for the lower equivalence bound
             alpha = 0.05)  # Alpha level for TOST and NHST