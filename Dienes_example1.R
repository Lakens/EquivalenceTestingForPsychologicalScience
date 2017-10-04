# TOST on the values from Gibson, Losee, and Vitello example

Gibson.m1 <- 0.59
Gibson.m2 <- 0.53
Gibson.sd1 <- 0.23
Gibson.sd2 <- 0.23
Gibson.n1 <- 52
Gibson.n2 <- 54

Shih.m1 <- 0.54
Shih.n2 <- 0.43
Shih.sd1 <- 0.17
Shih.sd2 <- 0.16
Shih.n1 <- 16
Shih.n2 <- 14

### Calculate subjective bound critera from original study data ###

# Original had 33% Power to detect effect of d=0.57:
library(pwr)
pwr.t.test(power=0.33,sig.level=0.05,n=15,type="two.sample",alternative="two.sided")

# Original study smallest detectable effect size
# Critical t for original study was:
crit_t = qt(1-.05/2, 29)
# So d becomes:
crit_d = crit_t*sqrt(1/16 + 1/14)

### TOST for the different bounds ###

# TOST for 33% Power rule:
library(devtools)
install_github("Lakens/TOSTER", force = TRUE)
TOSTtwo(m1 = Gibson.m1, m2 = Gibson.m2, sd1 = Gibson.sd1, sd2 = Gibson.sd2, n1 = Gibson.n1, n2 = Gibson.n2, low_eqbound_d = -0.57, high_eqbound_d = 0.57, alpha = .05)

# TOST for smallest detectable effect in original study
TOSTtwo(m1 = 0.59, m2 = 0.53, sd1 = 0.23, sd2 = 0.23, n1 = 52, n2 = 54, low_eqbound_d = -crit_d, high_eqbound_d = crit_d, alpha = .05)

#How much power did the replication have? 
#For 33% power value
powerTOSTtwo(alpha=0.05, N = Gibson.n1, low_eqbound_d=-0.57, high_eqbound_d=0.57)

#For crit_d (note this will almost always be less strict than 33% threshold)
powerTOSTtwo(alpha=0.05, N = Gibson.n1, low_eqbound_d=-crit_d, high_eqbound_d=crit_d)
